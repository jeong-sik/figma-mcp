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
const { execSync } = require('child_process');
const path = require('path');
const pixelmatchModule = require('pixelmatch');
const pixelmatch = pixelmatchModule.default || pixelmatchModule;

/**
 * PNG 파일을 pngjs 호환 포맷으로 변환 (macOS sips 사용)
 * Figma API에서 다운로드한 PNG는 pngjs가 읽지 못하는 경우가 있음
 */
function convertPNG(filePath) {
  const tmpPath = path.join('/tmp', `ssim_converted_${Date.now()}_${path.basename(filePath)}`);
  try {
    // macOS sips로 PNG 재인코딩 (표준 포맷으로 변환)
    execSync(`sips -s format png "${filePath}" --out "${tmpPath}" 2>/dev/null`, { stdio: 'pipe' });
    return tmpPath;
  } catch (e) {
    // sips 실패 시 ImageMagick 시도
    try {
      execSync(`convert "${filePath}" "${tmpPath}" 2>/dev/null`, { stdio: 'pipe' });
      return tmpPath;
    } catch (e2) {
      // 변환 실패 시 원본 반환
      return filePath;
    }
  }
}

/**
 * PNG 파일을 로드하고 디코드 (자동 변환 포함)
 */
function loadPNG(filePath) {
  let actualPath = filePath;
  let converted = false;

  try {
    // 먼저 직접 로드 시도
    const buffer = fs.readFileSync(filePath);
    return PNG.sync.read(buffer);
  } catch (e) {
    // 실패 시 변환 후 재시도
    actualPath = convertPNG(filePath);
    converted = true;
    const buffer = fs.readFileSync(actualPath);
    const result = PNG.sync.read(buffer);
    // 변환된 임시 파일 정리
    if (converted && actualPath !== filePath) {
      try { fs.unlinkSync(actualPath); } catch (_) {}
    }
    return result;
  }
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
 * ============== 논문 기반 고급 메트릭 (2025-01 추가) ==============
 */

/**
 * True SSIM (Wang et al., 2004)
 * "Image Quality Assessment: From Error Visibility to Structural Similarity"
 * IEEE Transactions on Image Processing, Vol. 13, No. 4
 *
 * SSIM(x,y) = [l(x,y)]^α · [c(x,y)]^β · [s(x,y)]^γ
 * - l(x,y) = (2μxμy + C1) / (μx² + μy² + C1)  // luminance
 * - c(x,y) = (2σxσy + C2) / (σx² + σy² + C2)  // contrast
 * - s(x,y) = (σxy + C3) / (σxσy + C3)          // structure
 *
 * With α=β=γ=1 and C3=C2/2:
 * SSIM(x,y) = (2μxμy + C1)(2σxy + C2) / (μx² + μy² + C1)(σx² + σy² + C2)
 */
function calculateTrueSSIM(img1Data, img2Data, width, height) {
  const K1 = 0.01, K2 = 0.03;
  const L = 255; // dynamic range
  const C1 = (K1 * L) ** 2;
  const C2 = (K2 * L) ** 2;
  const windowSize = 11;
  const sigma = 1.5;

  // Gaussian window weights (11x11) - normalized
  const weights = createGaussianWindow(windowSize, sigma);

  // Convert RGBA to grayscale (Y = 0.299R + 0.587G + 0.114B)
  const gray1 = new Float32Array(width * height);
  const gray2 = new Float32Array(width * height);
  for (let i = 0; i < width * height; i++) {
    const idx = i * 4;
    gray1[i] = 0.299 * img1Data[idx] + 0.587 * img1Data[idx + 1] + 0.114 * img1Data[idx + 2];
    gray2[i] = 0.299 * img2Data[idx] + 0.587 * img2Data[idx + 1] + 0.114 * img2Data[idx + 2];
  }

  let ssimSum = 0;
  let count = 0;
  const halfW = Math.floor(windowSize / 2);

  for (let y = halfW; y < height - halfW; y++) {
    for (let x = halfW; x < width - halfW; x++) {
      // Single-pass computation (numerically stable)
      // mu_x = sum(w * x), mu_y = sum(w * y)
      // sigma_x^2 = sum(w * x^2) - mu_x^2
      // sigma_xy = sum(w * x * y) - mu_x * mu_y
      let sumW1 = 0, sumW2 = 0;
      let sumW1Sq = 0, sumW2Sq = 0;
      let sumW12 = 0;

      for (let wy = -halfW; wy <= halfW; wy++) {
        for (let wx = -halfW; wx <= halfW; wx++) {
          const idx = (y + wy) * width + (x + wx);
          const w = weights[(wy + halfW) * windowSize + (wx + halfW)];
          const v1 = gray1[idx];
          const v2 = gray2[idx];

          sumW1 += w * v1;
          sumW2 += w * v2;
          sumW1Sq += w * v1 * v1;
          sumW2Sq += w * v2 * v2;
          sumW12 += w * v1 * v2;
        }
      }

      const mu1 = sumW1;
      const mu2 = sumW2;
      const sigma1Sq = sumW1Sq - mu1 * mu1;
      const sigma2Sq = sumW2Sq - mu2 * mu2;
      const sigma12 = sumW12 - mu1 * mu2;

      // SSIM formula (Wang et al. 2004)
      const numerator = (2 * mu1 * mu2 + C1) * (2 * sigma12 + C2);
      const denominator = (mu1 * mu1 + mu2 * mu2 + C1) * (sigma1Sq + sigma2Sq + C2);
      const localSSIM = numerator / denominator;

      ssimSum += localSSIM;
      count++;
    }
  }

  return count > 0 ? ssimSum / count : 0;
}

/**
 * MS-SSIM (Wang et al., 2003)
 * "Multi-scale Structural Similarity for Image Quality Assessment"
 * Asilomar Conference on Signals, Systems and Computers
 *
 * MS-SSIM = ∏(j=1 to M) [c_j(x,y)]^βj · [s_j(x,y)]^γj · [l_M(x,y)]^αM
 *
 * 5 scales with weights: [0.0448, 0.2856, 0.3001, 0.2363, 0.1333] (from paper)
 */
function calculateMSSSIM(img1Data, img2Data, width, height) {
  const scales = 5;
  const weights = [0.0448, 0.2856, 0.3001, 0.2363, 0.1333]; // Exponents from paper

  // Convert to grayscale
  let gray1 = new Float32Array(width * height);
  let gray2 = new Float32Array(width * height);
  for (let i = 0; i < width * height; i++) {
    const idx = i * 4;
    gray1[i] = 0.299 * img1Data[idx] + 0.587 * img1Data[idx + 1] + 0.114 * img1Data[idx + 2];
    gray2[i] = 0.299 * img2Data[idx] + 0.587 * img2Data[idx + 1] + 0.114 * img2Data[idx + 2];
  }

  let msSSIM = 1.0;
  let currentWidth = width;
  let currentHeight = height;

  for (let scale = 0; scale < scales; scale++) {
    // Minimum size check
    if (currentWidth < 11 || currentHeight < 11) break;

    // Compute contrast and structure at this scale
    const { contrast, structure, luminance } = computeCSL(gray1, gray2, currentWidth, currentHeight);

    if (scale === scales - 1) {
      // Last scale: include luminance
      msSSIM *= Math.pow(luminance, weights[scale]);
    }
    msSSIM *= Math.pow(contrast, weights[scale]) * Math.pow(structure, weights[scale]);

    // Downsample by 2x for next scale (average pooling)
    const newWidth = Math.floor(currentWidth / 2);
    const newHeight = Math.floor(currentHeight / 2);

    gray1 = downsample(gray1, currentWidth, currentHeight, newWidth, newHeight);
    gray2 = downsample(gray2, currentWidth, currentHeight, newWidth, newHeight);

    currentWidth = newWidth;
    currentHeight = newHeight;
  }

  return msSSIM;
}

/**
 * Compute Contrast, Structure, and Luminance components
 */
function computeCSL(gray1, gray2, width, height) {
  const K1 = 0.01, K2 = 0.03, L = 255;
  const C1 = (K1 * L) ** 2, C2 = (K2 * L) ** 2, C3 = C2 / 2;

  let mu1 = 0, mu2 = 0;
  const n = width * height;

  for (let i = 0; i < n; i++) {
    mu1 += gray1[i];
    mu2 += gray2[i];
  }
  mu1 /= n;
  mu2 /= n;

  let sigma1Sq = 0, sigma2Sq = 0, sigma12 = 0;
  for (let i = 0; i < n; i++) {
    const d1 = gray1[i] - mu1;
    const d2 = gray2[i] - mu2;
    sigma1Sq += d1 * d1;
    sigma2Sq += d2 * d2;
    sigma12 += d1 * d2;
  }
  sigma1Sq /= (n - 1);
  sigma2Sq /= (n - 1);
  sigma12 /= (n - 1);

  const sigma1 = Math.sqrt(sigma1Sq);
  const sigma2 = Math.sqrt(sigma2Sq);

  const luminance = (2 * mu1 * mu2 + C1) / (mu1 * mu1 + mu2 * mu2 + C1);
  const contrast = (2 * sigma1 * sigma2 + C2) / (sigma1Sq + sigma2Sq + C2);
  const structure = (sigma12 + C3) / (sigma1 * sigma2 + C3);

  return { luminance, contrast, structure };
}

/**
 * Create normalized Gaussian window
 */
function createGaussianWindow(size, sigma) {
  const weights = new Float32Array(size * size);
  const half = Math.floor(size / 2);
  let sum = 0;

  for (let y = 0; y < size; y++) {
    for (let x = 0; x < size; x++) {
      const dx = x - half, dy = y - half;
      const w = Math.exp(-(dx * dx + dy * dy) / (2 * sigma * sigma));
      weights[y * size + x] = w;
      sum += w;
    }
  }

  // Normalize
  for (let i = 0; i < weights.length; i++) {
    weights[i] /= sum;
  }

  return weights;
}

/**
 * Downsample image by factor of 2 (average pooling)
 */
function downsample(data, oldW, oldH, newW, newH) {
  const result = new Float32Array(newW * newH);
  for (let y = 0; y < newH; y++) {
    for (let x = 0; x < newW; x++) {
      const x2 = x * 2, y2 = y * 2;
      let sum = 0, count = 0;
      for (let dy = 0; dy < 2 && y2 + dy < oldH; dy++) {
        for (let dx = 0; dx < 2 && x2 + dx < oldW; dx++) {
          sum += data[(y2 + dy) * oldW + (x2 + dx)];
          count++;
        }
      }
      result[y * newW + x] = sum / count;
    }
  }
  return result;
}

/**
 * LPIPS placeholder - requires Python + PyTorch
 * Will be computed by external script if available
 */
function calculateLPIPS(path1, path2) {
  try {
    // Check for LPIPS Python script
    const lpipsScript = path.join(__dirname, 'lpips-compare.py');
    if (fs.existsSync(lpipsScript)) {
      const result = execSync(`python3 "${lpipsScript}" "${path1}" "${path2}"`, { encoding: 'utf8', timeout: 30000 });
      const parsed = JSON.parse(result.trim());
      return parsed.lpips || null;
    }
  } catch (e) {
    // LPIPS not available
  }
  return null; // Not available
}

/**
 * ============== 비교 함수 ==============
 */

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
  const pixelMatchSSIM = 1 - (diffPixels / totalPixels);  // Legacy: pixelmatch-based
  const mse = diffPixels / totalPixels;
  const psnr = mse === 0 ? 100 : 10 * Math.log10(1 / mse);

  // 🆕 True SSIM (Wang et al. 2004) - 논문 기반
  const trueSSIM = calculateTrueSSIM(img1Data, img2Data, width, height);

  // 🆕 MS-SSIM (Wang et al. 2003) - 다중 스케일
  const msSSIM = calculateMSSSIM(img1Data, img2Data, width, height);

  // 🆕 LPIPS (선택적 - Python 스크립트 필요)
  const lpips = calculateLPIPS(path1, path2);

  // 영역별 diff 분석
  const regions = analyzeRegions(diffBuffer, width, height);

  return {
    // Legacy metrics (backward compatible)
    ssim: parseFloat(trueSSIM.toFixed(6)),  // 🆕 Now uses true SSIM
    psnr: parseFloat(psnr.toFixed(2)),
    mse: parseFloat(mse.toFixed(6)),
    deltaE: parseFloat(avgDeltaE.toFixed(4)),
    diffPixels: diffPixels,
    totalPixels: totalPixels,
    width: width,
    height: height,
    regions: regions,

    // 🆕 Advanced metrics (논문 기반)
    advanced: {
      trueSSIM: parseFloat(trueSSIM.toFixed(6)),       // Wang et al. 2004
      msSSIM: parseFloat(msSSIM.toFixed(6)),           // Multi-scale SSIM
      pixelMatch: parseFloat(pixelMatchSSIM.toFixed(6)), // Legacy pixelmatch-based
      lpips: lpips,                                     // LPIPS (null if unavailable)
      _papers: {
        trueSSIM: "Wang et al. 2004 - IEEE TIP Vol.13 No.4",
        msSSIM: "Wang et al. 2003 - Asilomar Conference",
        lpips: "Zhang et al. 2018 - CVPR (requires Python)"
      }
    }
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
