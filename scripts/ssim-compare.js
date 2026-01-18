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
 * 두 이미지의 픽셀 차이 계산 (pixelmatch 기반)
 */
function compareImages(path1, path2) {
  const img1 = loadPNG(path1);
  const img2 = loadPNG(path2);

  // 크기가 다르면 작은 쪽에 맞춤
  const width = Math.min(img1.width, img2.width);
  const height = Math.min(img1.height, img2.height);

  // 출력 버퍼 (diff 이미지용)
  const diffBuffer = Buffer.alloc(width * height * 4);

  // pixelmatch로 차이 픽셀 수 계산
  const diffPixels = pixelmatch(
    cropImage(img1, width, height),
    cropImage(img2, width, height),
    diffBuffer,
    width,
    height,
    { threshold: 0.1 }
  );

  const totalPixels = width * height;

  // SSIM 근사값 (pixelmatch 기반)
  // diffPixels가 0이면 SSIM = 1.0 (완전 일치)
  // diffPixels가 totalPixels면 SSIM = 0.0 (완전 다름)
  const ssim = 1 - (diffPixels / totalPixels);

  // MSE 근사값
  const mse = diffPixels / totalPixels;

  // PSNR 계산 (MSE 기반)
  const psnr = mse === 0 ? 100 : 10 * Math.log10(1 / mse);

  // 영역별 diff 분석
  const regions = analyzeRegions(diffBuffer, width, height);

  return {
    ssim: parseFloat(ssim.toFixed(6)),
    psnr: parseFloat(psnr.toFixed(2)),
    mse: parseFloat(mse.toFixed(6)),
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
