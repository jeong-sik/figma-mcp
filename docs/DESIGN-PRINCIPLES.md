# Figma Design Principles for AI Agents

AI 에이전트가 figma-mcp로 UI를 생성할 때 반드시 따라야 하는 규칙.
코딩 컨벤션처럼 "해라/하지마라" 형태의 처방적(prescriptive) 규칙이다.

---

## 1. Grid & Spacing (8px 시스템)

모든 수치는 8의 배수를 사용한다. 4px는 예외적으로 허용한다.

| 용도 | 값 | 예시 |
|------|-----|------|
| 화면 좌우 패딩 | 24px | Frame → horizontalPadding: 24 |
| 섹션 간 간격 | 32px 또는 40px | 로고↔폼, 폼↔버튼 사이 |
| 요소 내부 간격 | 8px 또는 12px | 폼 필드 사이, 아이콘↔텍스트 |
| 미세 간격 | 4px | 라벨↔인풋, 캡션↔본문 |
| 컴포넌트 내부 패딩 | 16px | 버튼 좌우, 카드 내부 |

**규칙**:
- `gap`, `padding`, `margin`에 7, 13, 15, 22 같은 비정규 값을 사용하지 않는다.
- 화면 너비 375px 기준, 좌우 패딩 24px → 콘텐츠 영역 327px.
- 전체 화면 높이 812px (iPhone SE/13 mini 기준 safe area 포함).

---

## 2. Typography (텍스트 규칙)

### Scale

| 역할 | 크기 | 무게 | 행간 |
|------|------|------|------|
| 대제목 (H1) | 28px | Bold (700) | 36px |
| 중제목 (H2) | 22px | SemiBold (600) | 28px |
| 소제목 (H3) | 18px | SemiBold (600) | 24px |
| 본문 (Body) | 16px | Regular (400) | 24px |
| 보조 텍스트 | 14px | Regular (400) | 20px |
| 캡션 | 12px | Regular (400) | 16px |

### 규칙

- **모든 텍스트 노드에 fontSize, fontWeight, lineHeight를 명시한다.** 기본값에 의존하지 않는다.
- 버튼 텍스트는 SemiBold (600) 이상을 사용한다.
- placeholder 텍스트는 본문과 같은 크기, Regular 무게를 사용한다.
- 폰트 패밀리: `Inter`를 기본으로 사용한다. 한글은 `Pretendard` 또는 `Noto Sans KR`.
- letterSpacing: 14px 이하 텍스트에는 0, 16px 이상에는 -0.2px ~ -0.4px.

---

## 3. Color (색상 체계)

### 의미 기반 팔레트

| 역할 | HEX | 용도 |
|------|-----|------|
| Primary | #2563EB | CTA 버튼, 링크, 강조 |
| Primary Hover | #1D4ED8 | 버튼 hover 상태 |
| Text Primary | #111827 | 제목, 본문 |
| Text Secondary | #6B7280 | 보조 텍스트, placeholder |
| Text Tertiary | #9CA3AF | 비활성, 힌트 |
| Background | #FFFFFF | 기본 배경 |
| Surface | #F9FAFB | 카드, 인풋 배경 |
| Border | #E5E7EB | 구분선, 인풋 테두리 |
| Border Focus | #2563EB | 포커스 상태 테두리 |
| Error | #DC2626 | 에러 텍스트, 테두리 |
| Success | #16A34A | 성공 상태 |
| Disabled BG | #F3F4F6 | 비활성 배경 |
| Disabled Text | #D1D5DB | 비활성 텍스트 |

### 규칙

- **배경 위 텍스트는 WCAG AA 대비율 4.5:1 이상을 유지한다.**
- 흰 배경(#FFF)에 #9CA3AF 이하 밝기의 텍스트를 사용하지 않는다.
- 버튼 텍스트: Primary 배경에 #FFFFFF, Secondary 배경에 #111827.
- fill 색상을 지정할 때 `{r, g, b, a}` 0-1 범위를 사용한다 (Figma API 규격).
  - 예: #2563EB → `{r: 0.145, g: 0.388, b: 0.922, a: 1.0}`

---

## 4. Layout (레이아웃 규칙)

### Auto Layout 필수

- **모든 Frame에 Auto Layout을 적용한다.** Manual positioning은 최상위 Screen Frame에서만 허용.
- 방향: 화면 전체는 `VERTICAL`, 인라인 요소는 `HORIZONTAL`.

### 정렬

| 상황 | primaryAxisAlignItems | counterAxisAlignItems |
|------|----------------------|----------------------|
| 화면 전체 | `MIN` (위에서 아래) | `CENTER` (좌우 중앙) |
| 폼 컨테이너 | `MIN` | `STRETCH` (폼 필드 가로 채움) |
| 버튼 내부 | `CENTER` (텍스트 중앙) | `CENTER` |
| 인풋 내부 | `CENTER` | `MIN` (텍스트 왼쪽) |
| 헤더/네비게이션 | `CENTER` | `CENTER` |
| 카드 내부 | `MIN` | `STRETCH` |

### 규칙

- 텍스트가 시각적으로 중앙에 와야 하는 곳(버튼, 헤더, 로고)에서는 반드시 `CENTER` 정렬을 사용한다.
- 폼 필드(인풋, 텍스트에어리어)는 `STRETCH`로 컨테이너 폭을 채운다.
- `layoutSizingHorizontal: "FILL"`: 부모 폭을 채우는 자식에 사용한다.
- `layoutSizingVertical: "HUG"`: 컨텐츠에 맞춰 높이가 줄어드는 컨테이너에 사용한다.

---

## 5. Component Specs (컴포넌트 사양)

### Button (버튼)

| 속성 | Primary | Secondary | Text |
|------|---------|-----------|------|
| 높이 | 52px | 52px | auto (HUG) |
| 모서리 | 12px | 12px | 0 |
| 배경 | #2563EB | transparent | transparent |
| 테두리 | 없음 | 1px #E5E7EB | 없음 |
| 텍스트 색 | #FFFFFF | #111827 | #2563EB |
| 텍스트 크기 | 16px | 16px | 14px |
| 텍스트 무게 | SemiBold (600) | SemiBold (600) | Medium (500) |
| 좌우 패딩 | 24px | 24px | 8px |
| 가로 크기 | FILL (부모 폭 채움) | FILL | HUG |

**규칙**:
- **터치 타겟 최소 높이 48px.** 52px 권장.
- Primary 버튼은 화면당 1개만 사용한다.
- 버튼 텍스트는 항상 `CENTER`/`CENTER` 정렬이다.

### Input Field (인풋)

| 속성 | 값 |
|------|-----|
| 높이 | 52px |
| 모서리 | 10px |
| 배경 | #F9FAFB |
| 테두리 | 1px #E5E7EB |
| 좌우 패딩 | 16px |
| 상하 패딩 | 14px |
| placeholder 색 | #9CA3AF |
| placeholder 크기 | 16px Regular |
| 가로 크기 | FILL (부모 폭 채움) |

**규칙**:
- 인풋 위에 라벨을 배치할 경우 라벨↔인풋 간격 8px.
- 인풋 아래 에러 메시지 간격 4px, 색상 #DC2626, 크기 12px.
- Focus 상태: 테두리 #2563EB, 2px.

### Card (카드)

| 속성 | 값 |
|------|-----|
| 모서리 | 16px |
| 배경 | #FFFFFF |
| 그림자 | `{x: 0, y: 2, blur: 8, spread: 0, color: rgba(0,0,0,0.08)}` |
| 내부 패딩 | 20px |
| 가로 크기 | FILL |

---

## 6. 시각 위계 (Visual Hierarchy)

### 규칙

1. **화면의 주요 행동(CTA)을 가장 눈에 띄게 만든다.** Primary 버튼 = 가장 큰 색상 대비.
2. **위에서 아래로 중요도가 내려간다.** 로고/브랜드 → 핵심 컨텐츠 → 보조 액션 → 법적 텍스트.
3. **텍스트 크기로 위계를 표현한다.** 제목 > 본문 > 보조 > 캡션. 같은 크기의 텍스트가 3단계 이상 연속되면 위계가 불명확하다.
4. **색상으로 위계를 강화한다.** 중요 텍스트 = Text Primary, 보조 = Text Secondary, 힌트 = Text Tertiary.
5. **여백으로 그룹을 만든다.** 관련 요소 간 간격 < 비관련 요소 간 간격 (근접성 원리).

### 금지

- 같은 화면에 같은 크기/색/무게의 텍스트만 나열하지 않는다 (flat hierarchy).
- 3가지 이상의 Primary 색상 요소를 한 화면에 넣지 않는다.
- 배경과 거의 구별 불가능한 텍스트를 사용하지 않는다.

---

## 7. Screen Structure (화면 구조)

모바일 화면(375x812)을 만들 때 기본 구조:

```
Screen Frame (375 x 812, VERTICAL, bg: #FFFFFF)
├── Status Bar Area (375 x 44, reserved)
├── Navigation Bar (375 x 56, HORIZONTAL, CENTER/CENTER)
│   └── Title Text (18px SemiBold)
├── Content Area (375 x auto, VERTICAL, padding: 24px)
│   ├── Section 1
│   │   ├── Section Title (H2)
│   │   └── Section Content
│   ├── Spacer (gap: 32px)
│   └── Section 2
├── Spacer (layoutGrow: 1, pushes footer down)
└── Bottom Area (375 x auto, padding: 24px 24px 34px)
    └── Primary Button (FILL x 52)
```

### 규칙

- 최상위 Frame: `layoutMode: "VERTICAL"`, `primaryAxisAlignItems: "MIN"`, `counterAxisAlignItems: "CENTER"`.
- 하단 safe area: 34px 여백 (Home indicator 영역).
- Content와 Bottom을 분리하려면 `layoutGrow: 1`인 빈 Frame을 사이에 넣는다.
- 배경색은 최상위 Frame에서 한 번만 설정한다.

---

## 8. Agent Workflow Rules (에이전트 작업 규칙)

### Layout Agent (배치)

1. 최상위 Screen Frame을 먼저 만든다 (375x812, 배경색, Auto Layout).
2. 위→아래 순서로 영역 Frame을 만든다 (Status Bar → Nav → Content → Bottom).
3. 각 영역 Frame에 Auto Layout + padding + gap을 설정한다.
4. 자식 Frame의 `layoutSizingHorizontal`을 `FILL`로 설정한다.
5. **텍스트 노드를 만들지 않는다.** Frame 구조만 만든다.

### Design Agent (디자인)

1. Layout Agent가 만든 Frame ID 목록을 받는다.
2. 각 Frame에 fill, stroke, cornerRadius를 적용한다.
3. 텍스트 노드를 만들고 올바른 Frame 안에 배치한다.
4. 모든 텍스트에 fontSize, fontWeight, lineHeight, fills(색상)를 명시한다.
5. 버튼/인풋은 이 문서의 Component Specs를 그대로 따른다.
6. **임의의 값을 사용하지 않는다.** 이 문서에 정의된 값만 사용한다.

### Verification Agent (검증)

1. Export 후 시각적으로 확인한다.
2. 체크리스트:
   - [ ] 텍스트가 잘리거나 넘치지 않는가?
   - [ ] 버튼/인풋 높이가 48px 이상인가?
   - [ ] 텍스트 색상이 배경 대비 충분한가?
   - [ ] Auto Layout 정렬이 의도와 일치하는가?
   - [ ] 좌우 패딩이 24px로 일관되는가?
   - [ ] 요소 간 간격이 8의 배수인가?

---

## 9. API Value Reference (figma-mcp 값 변환)

### 색상 변환 (HEX → Figma RGBA 0-1)

```
#2563EB → {r: 0.145, g: 0.388, b: 0.922, a: 1.0}
#111827 → {r: 0.067, g: 0.094, b: 0.153, a: 1.0}
#6B7280 → {r: 0.420, g: 0.447, b: 0.502, a: 1.0}
#9CA3AF → {r: 0.612, g: 0.639, b: 0.686, a: 1.0}
#FFFFFF → {r: 1.0, g: 1.0, b: 1.0, a: 1.0}
#F9FAFB → {r: 0.976, g: 0.980, b: 0.984, a: 1.0}
#E5E7EB → {r: 0.898, g: 0.906, b: 0.922, a: 1.0}
#DC2626 → {r: 0.863, g: 0.149, b: 0.149, a: 1.0}
#F3F4F6 → {r: 0.953, g: 0.957, b: 0.965, a: 1.0}
```

### Auto Layout 속성

```json
{
  "layoutMode": "VERTICAL",
  "primaryAxisAlignItems": "MIN | CENTER | MAX | SPACE_BETWEEN",
  "counterAxisAlignItems": "MIN | CENTER | MAX | STRETCH",
  "paddingLeft": 24,
  "paddingRight": 24,
  "paddingTop": 16,
  "paddingBottom": 16,
  "itemSpacing": 12,
  "layoutSizingHorizontal": "FIXED | HUG | FILL",
  "layoutSizingVertical": "FIXED | HUG | FILL"
}
```

---

## 10. Anti-Patterns (하지 말 것)

| 금지 | 이유 | 대신 |
|------|------|------|
| 7, 13, 22px 같은 비정규 간격 | Grid 파괴 | 4, 8, 12, 16, 24, 32, 40px |
| 텍스트 색상 없이 기본값 사용 | 검은색(#000) 너무 강함 | #111827 (Text Primary) |
| 모든 텍스트 같은 크기 | 위계 없음 | Typography Scale 참조 |
| Manual positioning | 반응형 불가 | Auto Layout 사용 |
| 버튼 높이 36px | 터치 타겟 부족 | 최소 48px, 권장 52px |
| placeholder 없는 빈 인풋 | 용도 불명 | placeholder 텍스트 필수 |
| 배경과 비슷한 텍스트 색 | 읽기 불가 | WCAG AA 대비율 준수 |
| `STRETCH` 없이 인풋/버튼 배치 | 폭이 안 맞음 | `layoutSizingHorizontal: "FILL"` |
