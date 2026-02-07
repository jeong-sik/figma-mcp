#!/usr/bin/env node
/**
 * extract-html-metrics.js
 *
 * Extract DOM/layout/text/style metrics from HTML using Playwright.
 *
 * Usage: node extract-html-metrics.js <html-file-or-string> [width] [height]
 *
 * Security: blocks network + file requests by default to avoid SSRF/local file exfil.
 * Set FIGMA_MCP_RENDER_HTML_ALLOW_NETWORK=1 to allow external assets.
 */

const { chromium } = require('playwright');
const fs = require('fs');
const os = require('os');
const path = require('path');

function allowNetworkRequests() {
  const raw = (process.env.FIGMA_MCP_RENDER_HTML_ALLOW_NETWORK || '').trim().toLowerCase();
  return raw === '1' || raw === 'true' || raw === 'yes';
}

function isSafeInlineUrl(url) {
  return (
    url === 'about:blank' ||
    url.startsWith('data:') ||
    url.startsWith('blob:')
  );
}

function resolveExecutablePath() {
  const defaultPath = chromium.executablePath();
  const arch = process.arch;
  if (defaultPath && fs.existsSync(defaultPath)) {
    return defaultPath;
  }

  // On macOS arm64, Playwright may look for a mac-x64 path that does not exist.
  if (defaultPath && defaultPath.includes('chrome-headless-shell-mac-x64')) {
    const arm64Path = defaultPath.replace(
      'chrome-headless-shell-mac-x64',
      'chrome-headless-shell-mac-arm64'
    );
    if (fs.existsSync(arm64Path)) {
      return arm64Path;
    }
  }

  const cacheRoot = path.join(os.homedir(), 'Library', 'Caches', 'ms-playwright');
  if (!fs.existsSync(cacheRoot)) {
    return undefined;
  }

  const candidates = fs
    .readdirSync(cacheRoot)
    .filter((name) => name.startsWith('chromium_headless_shell-'))
    .sort()
    .reverse();

  for (const dir of candidates) {
    const baseDir = path.join(cacheRoot, dir);
    const arm64 = path.join(baseDir, 'chrome-headless-shell-mac-arm64', 'chrome-headless-shell');
    const x64 = path.join(baseDir, 'chrome-headless-shell-mac-x64', 'chrome-headless-shell');
    if (arch === 'arm64') {
      if (fs.existsSync(arm64)) {
        return arm64;
      }
      if (fs.existsSync(x64)) {
        return x64;
      }
    } else {
      if (fs.existsSync(x64)) {
        return x64;
      }
      if (fs.existsSync(arm64)) {
        return arm64;
      }
    }
  }

  return undefined;
}

async function extractHtmlMetrics(htmlPathOrString, width = 375, height = 812) {
  const launchOptions = { headless: true };
  const executablePath = resolveExecutablePath();
  if (executablePath) {
    launchOptions.executablePath = executablePath;
  }

  const browser = await chromium.launch(launchOptions);
  const page = await browser.newPage();

  // Security: block network + file requests by default to avoid SSRF/local file exfil.
  // If you need external assets, set FIGMA_MCP_RENDER_HTML_ALLOW_NETWORK=1 explicitly.
  if (!allowNetworkRequests()) {
    await page.route('**/*', (route) => {
      const url = route.request().url();
      if (isSafeInlineUrl(url)) {
        return route.continue();
      }
      return route.abort();
    });
  }

  await page.setViewportSize({ width, height });

  let htmlContent = htmlPathOrString;
  if (fs.existsSync(htmlPathOrString)) {
    htmlContent = fs.readFileSync(htmlPathOrString, 'utf-8');
  }

  await page.setContent(htmlContent, { waitUntil: 'domcontentloaded' });

  // Wait for fonts (best-effort)
  await page.evaluate(() => document.fonts.ready);
  await page.waitForTimeout(200);

  const metrics = await page.evaluate(() => {
    function clampText(s, maxLen) {
      const t = (s || '').trim().replace(/\s+/g, ' ');
      if (t.length <= maxLen) return t;
      return t.slice(0, maxLen) + '...<truncated>';
    }

    function isTransparent(color) {
      if (!color) return true;
      const c = String(color).trim().toLowerCase();
      return c === 'transparent' || c === 'rgba(0, 0, 0, 0)';
    }

    function visibleElement(el, style, rect) {
      if (!el || !style || !rect) return false;
      if (style.display === 'none' || style.visibility === 'hidden') return false;
      const op = parseFloat(style.opacity || '1');
      if (!Number.isFinite(op) || op <= 0) return false;
      if (rect.width <= 0.5 || rect.height <= 0.5) return false;
      return true;
    }

    const els = Array.from(document.querySelectorAll('body, body *'));
    const textNodes = [];
    const bgNodes = [];

    for (const el of els) {
      const style = window.getComputedStyle(el);
      const rect = el.getBoundingClientRect();
      if (!visibleElement(el, style, rect)) continue;

      const bbox = { x: rect.left, y: rect.top, width: rect.width, height: rect.height };
      const area = rect.width * rect.height;

      const text = clampText(el.innerText, 2000);
      if (text.length > 0) {
        textNodes.push({
          tag: el.tagName,
          text,
          bbox,
          styles: {
            fontSize: style.fontSize,
            fontWeight: style.fontWeight,
            fontFamily: style.fontFamily,
            color: style.color,
          },
        });
      }

      const bg = style.backgroundColor;
      if (!isTransparent(bg) && area >= 64) {
        bgNodes.push({
          tag: el.tagName,
          bbox,
          styles: {
            backgroundColor: bg,
            borderRadius: style.borderRadius,
          },
        });
      }
    }

    function byAreaDesc(a, b) {
      const areaA = (a.bbox.width || 0) * (a.bbox.height || 0);
      const areaB = (b.bbox.width || 0) * (b.bbox.height || 0);
      return areaB - areaA;
    }

    bgNodes.sort(byAreaDesc);
    textNodes.sort(byAreaDesc);

    const bgTop = bgNodes.slice(0, 30);
    const textTop = textNodes.slice(0, 120);

    const bodyStyle = window.getComputedStyle(document.body);
    const bodyRect = document.body.getBoundingClientRect();
    const fallbackRoot = {
      tag: 'BODY',
      bbox: { x: bodyRect.left, y: bodyRect.top, width: bodyRect.width, height: bodyRect.height },
      styles: { backgroundColor: bodyStyle.backgroundColor, borderRadius: bodyStyle.borderRadius },
    };
    const root = bgTop.length > 0 ? bgTop[0] : fallbackRoot;

    return {
      viewport: { width: window.innerWidth, height: window.innerHeight },
      root,
      text_nodes: textTop,
      bg_nodes: bgTop,
    };
  });

  await browser.close();

  return { success: true, ...metrics };
}

async function main() {
  const args = process.argv.slice(2);
  if (args.length < 1) {
    console.error('Usage: node extract-html-metrics.js <html-file-or-string> [width] [height]');
    process.exit(1);
  }

  const [htmlInput, widthStr, heightStr] = args;
  const width = widthStr ? parseInt(widthStr, 10) : 375;
  const height = heightStr ? parseInt(heightStr, 10) : 812;

  try {
    const result = await extractHtmlMetrics(htmlInput, width, height);
    console.log(JSON.stringify(result));
    process.exit(0);
  } catch (error) {
    console.error(JSON.stringify({ success: false, error: error && error.message ? error.message : String(error) }));
    process.exit(1);
  }
}

main();

