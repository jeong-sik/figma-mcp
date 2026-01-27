#!/usr/bin/env node
/**
 * render-html.js - HTML을 PNG로 렌더링하는 Playwright 스크립트
 *
 * Usage: node render-html.js <html-file> <output-png> [width] [height]
 *
 * Part of the Visual Feedback Loop for 95%+ Figma-to-Code accuracy.
 */

const { chromium } = require('playwright');
const fs = require('fs');
const os = require('os');
const path = require('path');

function resolveExecutablePath() {
  const defaultPath = chromium.executablePath();
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
    if (fs.existsSync(arm64)) {
      return arm64;
    }
    const x64 = path.join(baseDir, 'chrome-headless-shell-mac-x64', 'chrome-headless-shell');
    if (fs.existsSync(x64)) {
      return x64;
    }
  }

  return undefined;
}

async function renderHtmlToPng(htmlPath, outputPath, width = 375, height = 812) {
  const launchOptions = { headless: true };
  const executablePath = resolveExecutablePath();
  if (executablePath) {
    launchOptions.executablePath = executablePath;
  }
  const browser = await chromium.launch(launchOptions);
  const page = await browser.newPage();

  // 뷰포트 설정 (Figma frame 크기와 일치)
  await page.setViewportSize({ width, height });

  // HTML 파일 또는 문자열 로드
  if (fs.existsSync(htmlPath)) {
    const htmlContent = fs.readFileSync(htmlPath, 'utf-8');
    await page.setContent(htmlContent, { waitUntil: 'networkidle' });
  } else {
    // htmlPath가 실제 HTML 문자열인 경우
    await page.setContent(htmlPath, { waitUntil: 'networkidle' });
  }

  // 폰트 로딩 대기 (최대 3초)
  await page.evaluate(() => document.fonts.ready);
  await page.waitForTimeout(500); // 추가 렌더링 대기

  // PNG 스크린샷
  await page.screenshot({
    path: outputPath,
    fullPage: false,
    type: 'png'
  });

  await browser.close();

  return { success: true, output: outputPath, width, height };
}

// CLI 실행
async function main() {
  const args = process.argv.slice(2);

  if (args.length < 2) {
    console.error('Usage: node render-html.js <html-file-or-string> <output-png> [width] [height]');
    process.exit(1);
  }

  const [htmlInput, outputPath, widthStr, heightStr] = args;
  const width = widthStr ? parseInt(widthStr, 10) : 375;
  const height = heightStr ? parseInt(heightStr, 10) : 812;

  try {
    const result = await renderHtmlToPng(htmlInput, outputPath, width, height);
    console.log(JSON.stringify(result));
    process.exit(0);
  } catch (error) {
    console.error(JSON.stringify({ success: false, error: error.message }));
    process.exit(1);
  }
}

main();
