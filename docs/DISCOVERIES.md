# Figma-to-Code SSIM 최적화 발견사항

## 날짜: 2026-01-17

---

## 🔍 핵심 발견사항

### 1. 이미지 크기 불일치 문제
- **문제**: Figma export와 HTML 렌더 크기가 다르면 SSIM 비교 불가
- **원인**: Figma는 그림자를 위한 패딩을 추가함
  - 모달 크기: 400x193
  - Figma export (scale=2): 828x414 (실제 207x414 @ scale 1)
  - 추가 패딩: ~14px (7px 각 방향)
- **해결**: HTML에도 동일한 패딩 적용 필요

### 2. Scale 파라미터 영향
- `scale=1`: 원본 크기 (400x193 → 414x207 with shadow)
- `scale=2`: 2배 크기 (800x386 → 828x414 with shadow)
- **결론**: 비교 시 동일 스케일 사용 필수

### 3. 배경색 불일치 (진행 중)
- **현상**: HTML에 #808080 배경 적용 시 SSIM 8.9%로 급락
- **원인 추정**: Figma export의 배경이 단순 회색이 아닐 수 있음
  - 투명 배경 + 그림자가 캔버스에 렌더링되는 방식 차이
  - Figma 캔버스 기본 배경색 확인 필요

### 4. 그림자 렌더링 차이
- Figma: `box-shadow: 0 2px 7px rgba(0,0,0,0.3)` 추정
- 브라우저: 동일 CSS 적용해도 미세한 렌더링 차이 발생 가능
- **팁**: `overflow:hidden`이 그림자를 잘라낼 수 있음 → `overflow:visible` 사용

### 5. Playwright 렌더 스크립트 문법
```bash
# 올바른 사용법
node render-html.js <input.html> <output.png> <width> <height>

# 예시
node render-html.js step4.html step4.png 414 207
```

### 6. 배경색 통일 전략 (90% SSIM 달성 핵심!)
- **Figma export**: 투명 배경 (`srgba(0,0,0,0)`)
- **Playwright 렌더**: 투명을 `#F8F8F8`로 렌더링
- **해결책**: Figma 이미지를 Playwright 배경색으로 flatten
```bash
# Figma 투명 배경을 Playwright 색상으로 평탄화
magick figma.png -background "#F8F8F8" -flatten figma_flat.png

# 그 후 비교
magick compare -metric SSIM html.png figma_flat.png null: 2>&1
```
- **효과**: 85.9% → **90.1%** (4.2%p 향상)

### 7. SSIM 측정 방법 (중요!)
```bash
# ImageMagick compare 사용
magick compare -metric SSIM figma.png html.png null: 2>&1

# 출력 형식: "12345.6 (0.xxxxx)"
# - 첫 번째 숫자: 원시 차이값 (무시해도 됨)
# - 괄호 안 값: 정규화된 dissimilarity (0~1)
# - 실제 SSIM = 1 - 괄호 안 값
```

**해석 예시**:
- `0 (0)` → dissimilarity=0 → **SSIM = 100%** (동일 이미지)
- `6739.6 (0.10284)` → dissimilarity=0.10284 → **SSIM = 89.7%**
- `9274.19 (0.141515)` → dissimilarity=0.141515 → **SSIM = 85.8%**

**주의**: ImageMagick은 "similarity"가 아닌 "dissimilarity"를 출력함!

---

## 📊 SSIM 결과 기록

| Step | 접근법 | Dissim | **SSIM** | 비고 |
|------|--------|--------|----------|------|
| step1 | 절대 위치 | 0.769 | **23.1%** | 크기 불일치 |
| step2 | Flexbox | 0.769 | **23.1%** | 크기 불일치 |
| step4 | 패딩+회색배경 | 0.911 | **8.9%** | 배경색 불일치 |
| step4_cropped | 중앙 크롭 400x193 | 0.1415 | **85.8%** | 크기 맞춤 후 대폭 개선! |
| step5 | 투명배경 HTML | 0.1415 | **85.9%** | Playwright가 #F8F8F8로 렌더 |
| **step5 + flatten** | **배경색 통일** | **0.0989** | **90.1%** ✅ | **목표 달성!** |

**결론**:
1. 크기를 정확히 맞추면 SSIM이 급격히 향상됨 (23% → 85.8%)
2. **배경색 통일이 핵심!** Figma 투명 배경 → Playwright 색상으로 flatten (85.9% → 90.1%)

---

## 🎯 아이콘 처리 전략 (이전 세션 학습)
- ❌ 직접 SVG 작성: SSIM 63.9%
- ✅ Figma SVG export: SSIM 89.5%
- **결론**: 아이콘은 항상 `figma_export_image(format=svg)` 사용

---

## 📝 다음 시도 계획
1. Figma export 이미지의 실제 배경 픽셀값 확인
2. 투명 배경으로 HTML 렌더 후 동일 조건에서 비교
3. Figma export 옵션 중 배경 관련 설정 확인

---

## 🔧 도구 및 환경
- Figma MCP: figma_export_image, figma_image_similarity
- Playwright: render-html.js
- ImageMagick: compare -metric SSIM
- 작업 디렉토리: /tmp/figma-evolution/
