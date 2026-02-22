# Multi-Metric Similarity Proof Experiment

## Purpose

A/B comparison to determine whether metric-driven iteration (Treatment)
outperforms fixed iteration (Control) for Figma design-to-code fidelity.

## Hypothesis

> Metric-driven iteration with early stopping achieves equal or higher
> fidelity scores while using fewer iterations than fixed-depth iteration.

## Design

| Aspect | Control (C) | Treatment (T) |
|--------|------------|---------------|
| Iterations | Fixed 5 | Up to 10 (early stop) |
| Depths | 4, 8, 12, 16, 20 | 4, 8, 12, ... (dynamic) |
| Stop condition | Always runs all 5 | target_ssim=0.92, plateau, regression |
| Metrics collected | SSIM, delta_e, human_ssim | Same |

### Test Designs (10 specimens)

| ID | Name | Complexity | Est. Nodes |
|----|------|-----------|------------|
| D01 | Login Form | Simple | 12 |
| D02 | Empty State | Simple | 8 |
| D03 | Alert Banner | Simple | 15 |
| D04 | Card List | Medium | 35 |
| D05 | Profile Page | Medium | 28 |
| D06 | Settings Screen | Medium | 42 |
| D07 | Dashboard Widget | Medium | 38 |
| D08 | Full Dashboard | Complex | 75 |
| D09 | Chat Screen | Complex | 60 |
| D10 | Calendar View | Complex | 85 |

## Metrics

| Metric | Source | Range |
|--------|--------|-------|
| SSIM | `Figma_image_similarity.ssim` (8x8 window luma) | 0.0 - 1.0 |
| delta_e | `Ciede2000.delta_e` (CIEDE2000 color diff) | 0.0 - 100+ |
| human_ssim | `Visual_verifier.calculate_human_ssim` | 0.0 - 1.0 |

**human_ssim formula**: `ssim * (1.0 - min(1.0, delta_e / 50.0))`

Penalizes structural similarity when colors diverge significantly.

## Simulation Model

The simulation engine generates deterministic data without Figma API access.

**Convergence curve**: `ceiling * (1 - e^(-k * iteration))`

| Complexity | Ceiling | k (rate) | Noise scale |
|------------|---------|----------|-------------|
| Simple | 0.94 | 0.8 | 0.02 |
| Medium | 0.88 | 0.5 | 0.03 |
| Complex | 0.80 | 0.3 | 0.04 |

Noise is deterministic (seeded from `Hashtbl.hash design.id`).

## Early Stop Configuration (Treatment)

```ocaml
{ target_ssim = 0.92;
  plateau_threshold = 0.005;
  plateau_patience = 3;
  text_ceiling = 0.88;
  max_iterations = 10; }
```

Stop reasons: `TARGET`, `PLATEAU`, `TEXT_CEILING`, `REGRESSION`, `MAX_ITER`.

## Statistical Analysis

| Test | Purpose |
|------|---------|
| Paired t-test | Mean difference (parametric) |
| Wilcoxon signed-rank | Non-parametric alternative |
| Cohen's d | Effect size magnitude |
| Shapiro-Wilk | Normality check (n=10) |

### Success Criteria

1. **T SSIM > C SSIM** (mean), p < 0.05
2. **Cohen's d >= 0.5** (medium effect)
3. **T iterations <= C iterations** (efficiency)

## Usage

### Run Experiment (OCaml)

```bash
cd figma-mcp
dune exec test/proof_experiment.exe -- --mode simulate
dune exec test/proof_experiment.exe -- --mode simulate --csv results.csv
```

### Analyze Results (Python)

```bash
pip install pandas scipy matplotlib
python3 test/analyze_experiment.py
python3 test/analyze_experiment.py --csv results.csv --output-dir results/
```

### Output Files

| File | Description |
|------|-------------|
| `test/proof_experiment_results.csv` | Raw measurements (default) |
| `PROOF_EXPERIMENT_RESULTS.md` | Statistical report |
| `proof_experiment_boxplot.png` | SSIM distribution by complexity |
| `proof_experiment_convergence.png` | Per-design convergence curves |

## File Structure

```
test/
  proof_experiment.ml       # OCaml experiment runner
  analyze_experiment.py     # Python statistical analysis
docs/
  PROOF_EXPERIMENT.md       # This document
```

## Limitations

- Simulation mode uses synthetic data; live mode requires real Figma API credentials and design IDs.
- Sample size (n=10) limits statistical power. Results from simulation establish methodology, not production claims.
- human_ssim formula assumes linear delta_e penalty, which may not match perceptual uniformity at extremes.
