#!/usr/bin/env python3
"""
ssim_compare.py - NumPy 기반 SSIM 계산 (scikit-image 없이)

Usage: python3 ssim_compare.py <image1.png> <image2.png>
Output: JSON {"ssim": 0.95, "psnr": 35.2, "mse": 0.001}

Part of the Visual Feedback Loop for 95%+ Figma-to-Code accuracy.
"""

import sys
import json
import numpy as np
from PIL import Image


def load_image(path: str) -> np.ndarray:
    """이미지를 grayscale numpy array로 로드"""
    img = Image.open(path).convert('L')  # grayscale
    return np.array(img, dtype=np.float64)


def ssim(img1: np.ndarray, img2: np.ndarray,
         C1: float = 6.5025, C2: float = 58.5225) -> float:
    """
    Structural Similarity Index (SSIM) 계산

    Wang, Z., Bovik, A. C., Sheikh, H. R., & Simoncelli, E. P. (2004).
    "Image quality assessment: from error visibility to structural similarity."
    IEEE Transactions on Image Processing, 13(4), 600-612.
    """
    # 같은 크기로 맞춤
    if img1.shape != img2.shape:
        # 작은 쪽에 맞춤
        h = min(img1.shape[0], img2.shape[0])
        w = min(img1.shape[1], img2.shape[1])
        img1 = img1[:h, :w]
        img2 = img2[:h, :w]

    # Mean
    mu1 = img1.mean()
    mu2 = img2.mean()

    # Variance and Covariance
    sigma1_sq = ((img1 - mu1) ** 2).mean()
    sigma2_sq = ((img2 - mu2) ** 2).mean()
    sigma12 = ((img1 - mu1) * (img2 - mu2)).mean()

    # SSIM formula
    numerator = (2 * mu1 * mu2 + C1) * (2 * sigma12 + C2)
    denominator = (mu1 ** 2 + mu2 ** 2 + C1) * (sigma1_sq + sigma2_sq + C2)

    return numerator / denominator


def psnr(img1: np.ndarray, img2: np.ndarray) -> float:
    """Peak Signal-to-Noise Ratio (PSNR) 계산"""
    mse_val = mse(img1, img2)
    if mse_val == 0:
        return float('inf')
    max_pixel = 255.0
    return 10 * np.log10((max_pixel ** 2) / mse_val)


def mse(img1: np.ndarray, img2: np.ndarray) -> float:
    """Mean Squared Error 계산"""
    if img1.shape != img2.shape:
        h = min(img1.shape[0], img2.shape[0])
        w = min(img1.shape[1], img2.shape[1])
        img1 = img1[:h, :w]
        img2 = img2[:h, :w]

    return ((img1 - img2) ** 2).mean()


def compare_images(path1: str, path2: str) -> dict:
    """두 이미지 비교"""
    img1 = load_image(path1)
    img2 = load_image(path2)

    ssim_score = ssim(img1, img2)
    psnr_score = psnr(img1, img2)
    mse_score = mse(img1, img2)

    return {
        "ssim": round(ssim_score, 6),
        "psnr": round(psnr_score, 2) if psnr_score != float('inf') else 100.0,
        "mse": round(mse_score, 6)
    }


if __name__ == "__main__":
    if len(sys.argv) != 3:
        print(json.dumps({"error": "Usage: ssim_compare.py <image1> <image2>"}))
        sys.exit(1)

    try:
        result = compare_images(sys.argv[1], sys.argv[2])
        print(json.dumps(result))
    except Exception as e:
        print(json.dumps({"error": str(e)}))
        sys.exit(1)
