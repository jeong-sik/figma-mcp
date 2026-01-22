#!/usr/bin/env node
/**
 * pixel-perfect-analyzer.js - SSIM 차이 분석 + CSS 자동 보정 제안
 *
 * Usage: node pixel-perfect-analyzer.js <ssim-result.json> [--figma-dsl <dsl.json>]
 * Output: JSON with CSS correction suggestions
 *
 * Part of the Pixel-Perfect Loop for 99%+ Figma-to-Code accuracy.
 */

const fs = require('fs');

/**
 * Diff region 분석 결과로부터 문제 영역 식별
 */
function identifyProblemAreas(regions) {
  const problems = [];
  const threshold = 0.02; // 2% 이상 diff가 있으면 문제로 간주

  // Edge 분석 - padding/margin 문제
  if (regions.edges.top > threshold) {
    problems.push({ area: 'edge-top', severity: regions.edges.top, type: 'spacing', suggestion: 'padding-top 또는 margin-top 조정 필요' });
  }
  if (regions.edges.bottom > threshold) {
    problems.push({ area: 'edge-bottom', severity: regions.edges.bottom, type: 'spacing', suggestion: 'padding-bottom 또는 margin-bottom 조정 필요' });
  }
  if (regions.edges.left > threshold) {
    problems.push({ area: 'edge-left', severity: regions.edges.left, type: 'spacing', suggestion: 'padding-left 또는 margin-left 조정 필요' });
  }
  if (regions.edges.right > threshold) {
    problems.push({ area: 'edge-right', severity: regions.edges.right, type: 'spacing', suggestion: 'padding-right 또는 margin-right 조정 필요' });
  }

  // Quadrant 분석 - 레이아웃 정렬 문제
  const quadrantValues = Object.values(regions.quadrants);
  const maxQuadrant = Math.max(...quadrantValues);
  const minQuadrant = Math.min(...quadrantValues);
  if (maxQuadrant - minQuadrant > 0.03) { // 3% 이상 차이
    const worstQuadrant = Object.entries(regions.quadrants).find(([_, v]) => v === maxQuadrant);
    problems.push({
      area: `quadrant-${worstQuadrant[0]}`,
      severity: maxQuadrant,
      type: 'layout',
      suggestion: `${worstQuadrant[0]} 영역에 레이아웃 불일치 - flex align/justify 확인`
    });
  }

  // Strip 분석 - 섹션별 문제
  if (regions.strips.top > threshold * 2 && regions.strips.middle < threshold) {
    problems.push({ area: 'header', severity: regions.strips.top, type: 'section', suggestion: '헤더 영역 스타일 불일치' });
  }
  if (regions.strips.bottom > threshold * 2 && regions.strips.middle < threshold) {
    problems.push({ area: 'footer', severity: regions.strips.bottom, type: 'section', suggestion: '푸터 영역 스타일 불일치' });
  }

  return problems;
}

/**
 * Delta E 값으로부터 색상 문제 심각도 판단
 */
function analyzeColorDifference(deltaE) {
  // Delta E 해석:
  // 0-1: 차이 인지 불가
  // 1-2: 가까이서 봐야 인지
  // 2-3.5: 알아볼 수 있는 차이
  // 3.5-5: 명확한 차이
  // 5+: 다른 색상으로 인지

  if (deltaE < 1) {
    return { severity: 'excellent', message: '색상 거의 완벽 (ΔE < 1)' };
  } else if (deltaE < 2) {
    return { severity: 'good', message: '색상 양호 (ΔE 1-2)' };
  } else if (deltaE < 3.5) {
    return { severity: 'acceptable', message: '색상 수용 가능 (ΔE 2-3.5)' };
  } else if (deltaE < 5) {
    return { severity: 'warning', message: '색상 차이 명확 (ΔE 3.5-5) - 색상 코드 확인 필요' };
  } else {
    return { severity: 'critical', message: `색상 심각한 불일치 (ΔE ${deltaE.toFixed(1)}) - 색상 완전히 다름` };
  }
}

/**
 * CSS 자동 보정 제안 생성
 */
function generateCssCorrections(ssimResult, figmaDsl = null) {
  const corrections = [];
  const { ssim, deltaE, regions, diffPixels, totalPixels } = ssimResult;

  // SSIM 기반 전체 판단
  const diffRatio = diffPixels / totalPixels;

  // 1. 색상 보정
  const colorAnalysis = analyzeColorDifference(deltaE);
  if (colorAnalysis.severity === 'warning' || colorAnalysis.severity === 'critical') {
    corrections.push({
      type: 'color',
      priority: colorAnalysis.severity === 'critical' ? 1 : 2,
      message: colorAnalysis.message,
      css: figmaDsl ? extractColorCorrections(figmaDsl) : [
        '/* 색상 코드 확인 필요 */',
        '/* Figma에서 정확한 HEX/RGB 값 추출 후 적용 */',
        '/* 예: background: #RRGGBB; color: rgb(R, G, B); */'
      ]
    });
  }

  // 2. 영역별 문제 분석
  const problems = identifyProblemAreas(regions);

  // 간격 문제
  const spacingProblems = problems.filter(p => p.type === 'spacing');
  if (spacingProblems.length > 0) {
    const edgeSuggestions = spacingProblems.map(p => {
      const direction = p.area.replace('edge-', '');
      const delta = Math.round(p.severity * 100); // 대략적인 픽셀 추정
      return `/* ${direction}: ${delta}px 정도 조정 필요 */\n  /* padding-${direction}: Xpx; 또는 margin-${direction}: Xpx; */`;
    });
    corrections.push({
      type: 'spacing',
      priority: 2,
      message: `간격/여백 불일치 (${spacingProblems.map(p => p.area).join(', ')})`,
      css: edgeSuggestions
    });
  }

  // 레이아웃 문제
  const layoutProblems = problems.filter(p => p.type === 'layout');
  if (layoutProblems.length > 0) {
    corrections.push({
      type: 'layout',
      priority: 1,
      message: '레이아웃 정렬 불일치',
      css: [
        '/* Flex 컨테이너 정렬 확인 */',
        'display: flex;',
        'align-items: center;    /* 또는 flex-start, flex-end */',
        'justify-content: center; /* 또는 flex-start, space-between */'
      ]
    });
  }

  // 섹션 문제
  const sectionProblems = problems.filter(p => p.type === 'section');
  if (sectionProblems.length > 0) {
    corrections.push({
      type: 'section',
      priority: 2,
      message: `섹션별 스타일 불일치 (${sectionProblems.map(p => p.area).join(', ')})`,
      css: sectionProblems.map(p => `/* ${p.area} 영역 스타일 확인 필요 */`)
    });
  }

  // 3. SSIM 기반 전체 권장사항
  if (ssim >= 0.99) {
    corrections.unshift({
      type: 'summary',
      priority: 0,
      message: `🎯 SSIM ${(ssim * 100).toFixed(1)}% - 거의 완벽! 미세 조정만 필요`,
      css: ['/* 현재 구현이 매우 정확합니다 */']
    });
  } else if (ssim >= 0.95) {
    corrections.unshift({
      type: 'summary',
      priority: 0,
      message: `✅ SSIM ${(ssim * 100).toFixed(1)}% - 양호. 세부 조정 권장`,
      css: []
    });
  } else if (ssim >= 0.90) {
    corrections.unshift({
      type: 'summary',
      priority: 0,
      message: `⚠️ SSIM ${(ssim * 100).toFixed(1)}% - 개선 필요`,
      css: []
    });
  } else {
    corrections.unshift({
      type: 'summary',
      priority: 0,
      message: `❌ SSIM ${(ssim * 100).toFixed(1)}% - 주요 불일치. 레이아웃 재검토 필요`,
      css: ['/* 전체 레이아웃 구조 검토 필요 */']
    });
  }

  // 우선순위로 정렬
  corrections.sort((a, b) => a.priority - b.priority);

  return corrections;
}

/**
 * Figma DSL에서 색상 보정 추출 (선택적)
 */
function extractColorCorrections(dsl) {
  const corrections = [];

  // DSL에서 색상 정보 추출
  if (dsl.fills) {
    dsl.fills.forEach((fill, i) => {
      if (fill.color) {
        const { r, g, b } = fill.color;
        const hex = rgbToHex(r, g, b);
        corrections.push(`background-color: ${hex}; /* fill ${i + 1} */`);
      }
    });
  }

  if (dsl.strokes) {
    dsl.strokes.forEach((stroke, i) => {
      if (stroke.color) {
        const { r, g, b } = stroke.color;
        const hex = rgbToHex(r, g, b);
        corrections.push(`border-color: ${hex}; /* stroke ${i + 1} */`);
      }
    });
  }

  if (dsl.effects) {
    dsl.effects.filter(e => e.type === 'DROP_SHADOW').forEach((shadow, i) => {
      if (shadow.color) {
        const { r, g, b, a } = shadow.color;
        corrections.push(`box-shadow: ${shadow.offset?.x || 0}px ${shadow.offset?.y || 0}px ${shadow.radius || 0}px rgba(${Math.round(r*255)}, ${Math.round(g*255)}, ${Math.round(b*255)}, ${a || 1}); /* shadow ${i + 1} */`);
      }
    });
  }

  return corrections.length > 0 ? corrections : ['/* Figma DSL에서 색상 정보를 확인하세요 */'];
}

function rgbToHex(r, g, b) {
  const toHex = (c) => {
    const val = Math.round(c * 255);
    return val.toString(16).padStart(2, '0');
  };
  return `#${toHex(r)}${toHex(g)}${toHex(b)}`.toUpperCase();
}

/**
 * 다음 반복에서 집중해야 할 영역 추천
 */
function recommendNextFocus(ssimResult) {
  const { regions, deltaE } = ssimResult;
  const recommendations = [];

  // 가장 심각한 영역 찾기
  const allAreas = [
    ...Object.entries(regions.edges).map(([k, v]) => ({ area: `edge-${k}`, diff: v })),
    ...Object.entries(regions.quadrants).map(([k, v]) => ({ area: `quadrant-${k}`, diff: v })),
    ...Object.entries(regions.strips).map(([k, v]) => ({ area: `strip-${k}`, diff: v }))
  ];

  const sorted = allAreas.sort((a, b) => b.diff - a.diff);
  const worst = sorted.slice(0, 3);

  recommendations.push({
    priority: 'high',
    focus: worst.map(w => w.area),
    message: `가장 큰 차이 영역: ${worst.map(w => `${w.area}(${(w.diff * 100).toFixed(1)}%)`).join(', ')}`
  });

  if (deltaE > 2) {
    recommendations.push({
      priority: 'medium',
      focus: ['colors'],
      message: `색상 정확도 개선 필요 (ΔE: ${deltaE.toFixed(2)})`
    });
  }

  return recommendations;
}

/**
 * Pixel-Perfect 수렴 전략 생성
 */
function generateConvergenceStrategy(currentSsim, targetSsim = 0.99) {
  const gap = targetSsim - currentSsim;
  const iterations = Math.ceil(gap / 0.02); // 반복당 2% 개선 가정

  return {
    currentSsim: (currentSsim * 100).toFixed(1) + '%',
    targetSsim: (targetSsim * 100).toFixed(1) + '%',
    gap: (gap * 100).toFixed(1) + '%',
    estimatedIterations: Math.max(1, Math.min(iterations, 10)),
    strategy: gap < 0.02 ? 'fine-tuning' : gap < 0.05 ? 'targeted-fixes' : 'major-revision',
    reachable: gap < 0.15 // 15% 이상 차이는 수렴 어려움
  };
}

// CLI
function main() {
  const args = process.argv.slice(2);

  if (args.length < 1) {
    console.log(JSON.stringify({ error: 'Usage: pixel-perfect-analyzer.js <ssim-result.json> [--figma-dsl <dsl.json>]' }));
    process.exit(1);
  }

  try {
    // SSIM 결과 로드
    let ssimResult;
    if (args[0].startsWith('{')) {
      // JSON 문자열로 직접 전달된 경우
      ssimResult = JSON.parse(args[0]);
    } else {
      // 파일 경로로 전달된 경우
      ssimResult = JSON.parse(fs.readFileSync(args[0], 'utf8'));
    }

    // Figma DSL 로드 (선택적)
    let figmaDsl = null;
    const dslIndex = args.indexOf('--figma-dsl');
    if (dslIndex !== -1 && args[dslIndex + 1]) {
      try {
        figmaDsl = JSON.parse(fs.readFileSync(args[dslIndex + 1], 'utf8'));
      } catch (e) {
        // DSL 로드 실패 무시
      }
    }

    // 분석 수행
    const corrections = generateCssCorrections(ssimResult, figmaDsl);
    const nextFocus = recommendNextFocus(ssimResult);
    const convergence = generateConvergenceStrategy(ssimResult.ssim);

    const result = {
      analysis: {
        ssim: ssimResult.ssim,
        deltaE: ssimResult.deltaE,
        diffPixels: ssimResult.diffPixels,
        totalPixels: ssimResult.totalPixels,
        colorStatus: analyzeColorDifference(ssimResult.deltaE)
      },
      corrections,
      nextFocus,
      convergence,
      summary: {
        ready: ssimResult.ssim >= 0.99,
        acceptable: ssimResult.ssim >= 0.95,
        needsWork: ssimResult.ssim < 0.90
      }
    };

    console.log(JSON.stringify(result, null, 2));
  } catch (error) {
    console.log(JSON.stringify({ error: error.message }));
    process.exit(1);
  }
}

main();
