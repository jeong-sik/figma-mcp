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

---

## 🎯 95% SSIM 도전 발견사항 (2026-01-22)

### 핵심 브레이크스루: 레이아웃/텍스트 분리 측정

**문제 상황**: v1~v5 반복 시도에도 72% SSIM에서 정체

| 버전 | SSIM | 변경 |
|------|------|------|
| v1 | 65.5% | 초기 구현 |
| v2 | 72.5% | DSL 정확도 ↑ |
| v3-v5 | 72.1~72.5% | **정체** |

**관점 전환**:
> "폰트와 아이콘을 ssim에서 제외하고 큰 레이아웃 덩어리부터 잡는다면?"

**결과**:
```
┌─────────────────────────────────────────────────┐
│  전체 비교:        SSIM 72.5%  (막힘)           │
│  텍스트 마스킹:    SSIM 95.7%  ✅               │
│  MS-SSIM:         99.2%                        │
│  Pixel Match:     99.2%                        │
└─────────────────────────────────────────────────┘
```

**결론**: 레이아웃 구조는 완벽, 문제는 오직 텍스트/아이콘 렌더링 차이

---

### 95% SSIM이 어려운 이유 (현실적 기대치)

| 원인 | 설명 |
|------|------|
| 폰트 렌더링 엔진 | Figma ≠ 브라우저 (서브픽셀, 힌팅) |
| 색상 공간 | Figma=Display P3, 브라우저=sRGB |
| 레이아웃 엔진 | Auto Layout ≠ CSS Flexbox (미세 차이) |
| SVG 렌더링 | 벡터 → 래스터 변환 차이 |

**현실적 목표**:
- 자동 변환: **70-80%** SSIM
- 수동 미세조정: **80-85%** SSIM
- 픽셀 퍼펙트 수작업: **90%+** SSIM (매우 어려움)
- 레이아웃만 측정: **95%+** 가능!

---

### 높이 계산 오류 발견 (버튼 잘림 문제)

**증상**: 버튼이 화면에 안 보임
```
버튼 중앙 (320, 370):
- Figma: RGB(32, 141, 249) = 파란색 버튼 ✅
- HTML:  RGB(255, 255, 255) = 흰색 배경 ❌
```

**원인**: 콘텐츠 높이 > 뷰포트
```
전체 콘텐츠: 441px
뷰포트:     398px
→ 버튼이 43px 아래로 잘림
```

**해결**: DSL 높이값 정확히 적용
```css
/* DSL에서 추출한 정확한 높이 */
.header  { height: 44px; }
.con     { height: 276px; }  /* 이게 빠져서 문제! */
.divider { height: 1px; }
.notice  { height: 44px; }
.btn-area { height: 33px; }
/* Total: 398px */
```

---

### DSL 정밀 분석의 중요성

**발견된 미세 오류들**:
```
radius: 10px  (내가 4px로 잘못 설정 → 차이 발생)
padding: 12/16/12/16  (TRBL 순서 주의!)
gap: 8px  (가로/세로 배치 결정)
```

**체크리스트**:
- [ ] DSL에서 정확한 radius 추출
- [ ] padding은 TRBL 순서로 확인
- [ ] 고정 height가 있는지 확인
- [ ] gap 값으로 레이아웃 방향 확인

---

### 분리 측정 전략 (권장)

막혔을 때 "전체"를 보지 말고 **분리해서** 측정:

```python
# 1. 텍스트/아이콘 영역 마스킹
mask_regions = [
    (x1, y1, x2, y2),  # 텍스트 영역
    (x3, y3, x4, y4),  # 아이콘 영역
]

# 2. 마스킹된 이미지로 레이아웃만 비교
layout_ssim = compare_masked(figma_img, html_img, mask_regions)

# 3. 결과 해석
if layout_ssim > 0.95:
    print("레이아웃 OK → 텍스트/아이콘만 수정")
else:
    print("레이아웃 문제 → DSL 다시 분석")
```

**핵심 원칙**:
> 어디가 OK이고 어디가 문제인지 분리하면 명확해진다
