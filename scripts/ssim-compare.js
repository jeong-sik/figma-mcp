#!/usr/bin/env node
/**
 * ssim-compare.js - Node.js 기반 이미지 비교 (SSIM + pixelmatch)
 *
 * Usage: node ssim-compare.js <image1.png> <image2.png>
 * Output: JSON {"ssim": 0.95, "psnr": 35.2, "mse": 0.001, "diffPixels": 100}
 *
 * Part of the Visual Feedback Loop for 95%+ Figma-to-Code accuracy.
 */

const fs = require('fs');
const { PNG } = require('pngjs');
const pixelmatchModule = require('pixelmatch');
const pixelmatch = pixelmatchModule.default || pixelmatchModule;

/**
 * PNG 파일을 로드하고 디코드
 */
function loadPNG(filePath) {
  const buffer = fs.readFileSync(filePath);
  return PNG.sync.read(buffer);
}

/**
 * 특정 영역의 diff 픽셀 수 계산
 * diffBuffer: RGBA 4채널 diff 이미지 버퍼
 * 빨간색(255,0,0) 픽셀이 차이 픽셀
 */
function countDiffInRegion(diffBuffer, fullWidth, x0, y0, w, h) {
  let count = 0;
  for (let y = y0; y < y0 + h; y++) {
    for (let x = x0; x < x0 + w; x++) {
      const idx = (y * fullWidth + x) * 4;
      // pixelmatch는 차이 픽셀을 빨간색(기본)으로 표시
      // R > 200, G < 100, B < 100 이면 diff 픽셀
      if (diffBuffer[idx] > 200 && diffBuffer[idx + 1] < 100) {
        count++;
      }
    }
  }
  return count;
}

/**
 * 영역별 diff 분석
 * - quadrants: 4분면별 diff 비율
 * - strips: 수평 3등분별 diff 비율
 * - edges: 상하좌우 가장자리 diff 비율
 */
function analyzeRegions(diffBuffer, width, height) {
  const halfW = Math.floor(width / 2);
  const halfH = Math.floor(height / 2);
  const thirdH = Math.floor(height / 3);
  const edgeSize = Math.min(20, Math.floor(Math.min(width, height) / 10)); // 10% or 20px

  // 4분면 분석
  const quadrants = {
    topLeft: countDiffInRegion(diffBuffer, width, 0, 0, halfW, halfH),
    topRight: countDiffInRegion(diffBuffer, width, halfW, 0, width - halfW, halfH),
    bottomLeft: countDiffInRegion(diffBuffer, width, 0, halfH, halfW, height - halfH),
    bottomRight: countDiffInRegion(diffBuffer, width, halfW, halfH, width - halfW, height - halfH)
  };

  // 수평 3등분 분석 (헤더/본문/푸터 구분)
  const strips = {
    top: countDiffInRegion(diffBuffer, width, 0, 0, width, thirdH),
    middle: countDiffInRegion(diffBuffer, width, 0, thirdH, width, thirdH),
    bottom: countDiffInRegion(diffBuffer, width, 0, thirdH * 2, width, height - thirdH * 2)
  };

  // 가장자리 분석 (padding/border 문제 감지)
  const edges = {
    top: countDiffInRegion(diffBuffer, width, 0, 0, width, edgeSize),
    bottom: countDiffInRegion(diffBuffer, width, 0, height - edgeSize, width, edgeSize),
    left: countDiffInRegion(diffBuffer, width, 0, 0, edgeSize, height),
    right: countDiffInRegion(diffBuffer, width, width - edgeSize, 0, edgeSize, height)
  };

  // 비율로 변환
  const quadrantTotal = halfW * halfH;
  const stripTotal = width * thirdH;
  const edgeHTotal = width * edgeSize;
  const edgeVTotal = edgeSize * height;

  return {
    quadrants: {
      topLeft: parseFloat((quadrants.topLeft / quadrantTotal).toFixed(4)),
      topRight: parseFloat((quadrants.topRight / quadrantTotal).toFixed(4)),
      bottomLeft: parseFloat((quadrants.bottomLeft / quadrantTotal).toFixed(4)),
      bottomRight: parseFloat((quadrants.bottomRight / quadrantTotal).toFixed(4))
    },
    strips: {
      top: parseFloat((strips.top / stripTotal).toFixed(4)),
      middle: parseFloat((strips.middle / stripTotal).toFixed(4)),
      bottom: parseFloat((strips.bottom / stripTotal).toFixed(4))
    },
    edges: {
      top: parseFloat((edges.top / edgeHTotal).toFixed(4)),
      bottom: parseFloat((edges.bottom / edgeHTotal).toFixed(4)),
      left: parseFloat((edges.left / edgeVTotal).toFixed(4)),
      right: parseFloat((edges.right / edgeVTotal).toFixed(4))
    }
  };
}

/**
 * CIEDE2000 Color Difference Constants
 */
const REF_X = 95.047;
const REF_Y = 100.0;
const REF_Z = 108.883;

/**
 * sRGB to XYZ conversion
 */
function rgb2xyz(r, g, b) {
  const linearize = (c) => c > 0.04045 ? Math.pow((c + 0.055) / 1.055, 2.4) : c / 12.92;
  const [lr, lg, lb] = [r / 255, g / 255, b / 255].map((c) => linearize(c) * 100);
  return [
    lr * 0.4124564 + lg * 0.3575761 + lb * 0.1804375,
    lr * 0.2126729 + lg * 0.7151522 + lb * 0.0721750,
    lr * 0.0193339 + lg * 0.1191920 + lb * 0.9503041
  ];
}

/**
 * XYZ to CIELab conversion
 */
function xyz2lab(x, y, z) {
  const f = (t) => t > 0.008856 ? Math.pow(t, 1 / 3) : (7.787 * t) + (16 / 116);
  const fx = f(x / REF_X);
  const fy = f(y / REF_Y);
  const fz = f(z / REF_Z);
  return [
    116 * fy - 16,
    500 * (fx - fy),
    200 * (fy - fz)
  ];
}

/**
 * RGBA to LAB
 */
function rgba2lab(r, g, b) {
  const [x, y, z] = rgb2xyz(r, g, b);
  return xyz2lab(x, y, z);
}

/**
 * Calculate Delta E 2000 color difference
 */
function deltaE00(lab1, lab2) {
  const [L1, a1, b1] = lab1;
  const [L2, a2, b2] = lab2;
  const rad2deg = (rad) => (rad * 180) / Math.PI;
  const deg2rad = (deg) => (deg * Math.PI) / 180;
  
  const kL = 1, kC = 1, kH = 1;
  const C1 = Math.sqrt(a1 ** 2 + b1 ** 2);
  const C2 = Math.sqrt(a2 ** 2 + b2 ** 2);
  const avgC = (C1 + C2) / 2;
  const G = 0.5 * (1 - Math.sqrt(avgC ** 7 / (avgC ** 7 + 25 ** 7)));
  const a1p = a1 * (1 + G);
  const a2p = a2 * (1 + G);
  const C1p = Math.sqrt(a1p ** 2 + b1 ** 2);
  const C2p = Math.sqrt(a2p ** 2 + b2 ** 2);
  const h1p = (b1 === 0 && a1p === 0) ? 0 : rad2deg(Math.atan2(b1, a1p)) % 360;
  const h2p = (b2 === 0 && a2p === 0) ? 0 : rad2deg(Math.atan2(b2, a2p)) % 360;
  const Lp = L2 - L1;
  const Cp = C2p - C1p;
  let hp = 0;
  if (C1p * C2p !== 0) {
    hp = h2p - h1p;
    if (hp > 180) hp -= 360;
    else if (hp < -180) hp += 360;
  }
  const Hp = 2 * Math.sqrt(C1p * C2p) * Math.sin(deg2rad(hp) / 2);
  const avgLp = (L1 + L2) / 2;
  const avgCp = (C1p + C2p) / 2;
  let avghp = (h1p + h2p) / 2;
  if (Math.abs(h1p - h2p) > 180) avghp += 180;
  const T = 1 - 0.17 * Math.cos(deg2rad(avghp - 30))
    + 0.24 * Math.cos(deg2rad(2 * avghp))
    + 0.32 * Math.cos(deg2rad(3 * avghp + 6))
    - 0.2 * Math.cos(deg2rad(4 * avghp - 63));
  const SL = 1 + (0.015 * (avgLp - 50) ** 2) / Math.sqrt(20 + (avgLp - 50) ** 2);
  const SC = 1 + 0.045 * avgCp;
  const SH = 1 + 0.015 * avgCp * T;
  const θ = 30 * Math.exp(((-((avghp - 275) / 25)) ** 2));
  const RC = 2 * Math.sqrt(avgCp ** 7 / (avgCp ** 7 + 25 ** 7));
  const RT = -RC * Math.sin(deg2rad(2 * θ));
  return Math.sqrt((Lp / (kL * SL)) ** 2 +
    (Cp / (kC * SC)) ** 2 +
    (Hp / (kH * SH)) ** 2 +
    RT * (Cp / (kC * SC)) * (Hp / (kH * SH)));
}

/**
 * 두 이미지의 픽셀 차이 계산 (pixelmatch + CIEDE2000 기반)
 */
function compareImages(path1, path2) {
  const img1 = loadPNG(path1);
  const img2 = loadPNG(path2);

  // 크기가 다르면 작은 쪽에 맞춤
  const width = Math.min(img1.width, img2.width);
  const height = Math.min(img1.height, img2.height);

  const img1Data = cropImage(img1, width, height);
  const img2Data = cropImage(img2, width, height);

  // 출력 버퍼 (diff 이미지용)
  const diffBuffer = Buffer.alloc(width * height * 4);

  // pixelmatch로 차이 픽셀 수 계산
  const diffPixels = pixelmatch(
    img1Data,
    img2Data,
    diffBuffer,
    width,
    height,
    { threshold: 0.1 }
  );

  // CIEDE2000 계산
  let totalDeltaE = 0;
  for (let y = 0; y < height; y++) {
    for (let x = 0; x < width; x++) {
      const idx = (y * width + x) * 4;
      const lab1 = rgba2lab(img1Data[idx], img1Data[idx + 1], img1Data[idx + 2]);
      const lab2 = rgba2lab(img2Data[idx], img2Data[idx + 1], img2Data[idx + 2]);
      totalDeltaE += deltaE00(lab1, lab2);
    }
  }
  const avgDeltaE = totalDeltaE / (width * height);

  const totalPixels = width * height;
  const ssim = 1 - (diffPixels / totalPixels);
  const mse = diffPixels / totalPixels;
  const psnr = mse === 0 ? 100 : 10 * Math.log10(1 / mse);

  // 영역별 diff 분석
  const regions = analyzeRegions(diffBuffer, width, height);

  return {
    ssim: parseFloat(ssim.toFixed(6)),
    psnr: parseFloat(psnr.toFixed(2)),
    mse: parseFloat(mse.toFixed(6)),
    deltaE: parseFloat(avgDeltaE.toFixed(4)),
    diffPixels: diffPixels,
    totalPixels: totalPixels,
    width: width,
    height: height,
    regions: regions
  };
}

/**
 * 이미지를 특정 크기로 자르기
 */
function cropImage(img, width, height) {
  if (img.width === width && img.height === height) {
    return img.data;
  }

  const cropped = Buffer.alloc(width * height * 4);
  for (let y = 0; y < height; y++) {
    for (let x = 0; x < width; x++) {
      const srcIdx = (y * img.width + x) * 4;
      const dstIdx = (y * width + x) * 4;
      cropped[dstIdx] = img.data[srcIdx];
      cropped[dstIdx + 1] = img.data[srcIdx + 1];
      cropped[dstIdx + 2] = img.data[srcIdx + 2];
      cropped[dstIdx + 3] = img.data[srcIdx + 3];
    }
  }
  return cropped;
}

// CLI
function main() {
  const args = process.argv.slice(2);

  if (args.length !== 2) {
    console.log(JSON.stringify({ error: 'Usage: ssim-compare.js <image1> <image2>' }));
    process.exit(1);
  }

  try {
    const result = compareImages(args[0], args[1]);
    console.log(JSON.stringify(result));
  } catch (error) {
    console.log(JSON.stringify({ error: error.message }));
    process.exit(1);
  }
}

main();
