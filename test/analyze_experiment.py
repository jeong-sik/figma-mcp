#!/usr/bin/env python3
"""Statistical analysis for Multi-Metric Similarity proof experiment.

Reads CSV output from proof_experiment.ml and produces:
1. Paired t-test (or Wilcoxon signed-rank if non-normal)
2. Cohen's d effect size
3. Boxplot: C vs T final SSIM by complexity
4. Convergence curves per design

Usage:
    python3 test/analyze_experiment.py [--csv path] [--output-dir path]
"""

import argparse
import csv
import os
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Optional


@dataclass(frozen=True, slots=True)
class Measurement:
    design_id: str
    design_name: str
    complexity: str
    group: str
    iteration: int
    depth: int
    ssim: float
    delta_e: float
    human_ssim: float
    fidelity_score: float
    stopped: bool
    stop_reason: str


def load_csv(path: str) -> list[Measurement]:
    """Load experiment CSV into typed measurements."""
    measurements: list[Measurement] = []
    with open(path) as f:
        reader = csv.DictReader(f)
        for row in reader:
            measurements.append(Measurement(
                design_id=row["design_id"],
                design_name=row["design_name"],
                complexity=row["complexity"],
                group=row["group"],
                iteration=int(row["iteration"]),
                depth=int(row["depth"]),
                ssim=float(row["ssim"]),
                delta_e=float(row["delta_e"]),
                human_ssim=float(row["human_ssim"]),
                fidelity_score=float(row["fidelity_score"]),
                stopped=row["stopped"].lower() == "true",
                stop_reason=row["stop_reason"],
            ))
    return measurements


def get_final_scores(
    measurements: list[Measurement], group: str
) -> dict[str, Measurement]:
    """Get last measurement per design for a group."""
    finals: dict[str, Measurement] = {}
    for m in measurements:
        if m.group == group:
            if m.design_id not in finals or m.iteration > finals[m.design_id].iteration:
                finals[m.design_id] = m
    return finals


def cohens_d(x: list[float], y: list[float]) -> float:
    """Cohen's d for paired samples (x - y)."""
    import math
    diffs = [a - b for a, b in zip(x, y)]
    n = len(diffs)
    if n == 0:
        return 0.0
    mean_d = sum(diffs) / n
    if n == 1:
        return float("inf") if mean_d != 0 else 0.0
    var_d = sum((d - mean_d) ** 2 for d in diffs) / (n - 1)
    sd_d = math.sqrt(var_d)
    if sd_d == 0:
        return float("inf") if mean_d != 0 else 0.0
    return mean_d / sd_d


def run_statistical_tests(
    c_ssims: list[float],
    t_ssims: list[float],
    c_hssims: list[float],
    t_hssims: list[float],
) -> dict[str, object]:
    """Run paired t-test and Wilcoxon signed-rank test."""
    try:
        from scipy import stats
    except ImportError:
        print("WARNING: scipy not installed. Install with: pip install scipy")
        print("         Skipping statistical tests (p-values).\n")
        return {
            "ssim_d": cohens_d(t_ssims, c_ssims),
            "hssim_d": cohens_d(t_hssims, c_hssims),
        }

    results: dict[str, object] = {}

    # Paired t-test on SSIM
    t_stat, p_value = stats.ttest_rel(t_ssims, c_ssims)
    results["ssim_ttest_t"] = t_stat
    results["ssim_ttest_p"] = p_value

    # Wilcoxon signed-rank (non-parametric alternative)
    try:
        w_stat, w_p = stats.wilcoxon(
            [t - c for t, c in zip(t_ssims, c_ssims)],
            alternative="greater",
        )
        results["ssim_wilcoxon_w"] = w_stat
        results["ssim_wilcoxon_p"] = w_p
    except ValueError as e:
        results["ssim_wilcoxon_error"] = str(e)

    # Paired t-test on human_ssim
    t_stat_h, p_value_h = stats.ttest_rel(t_hssims, c_hssims)
    results["hssim_ttest_t"] = t_stat_h
    results["hssim_ttest_p"] = p_value_h

    # Effect sizes
    results["ssim_d"] = cohens_d(t_ssims, c_ssims)
    results["hssim_d"] = cohens_d(t_hssims, c_hssims)

    # Normality check (Shapiro-Wilk on differences)
    diffs = [t - c for t, c in zip(t_ssims, c_ssims)]
    if len(diffs) >= 3:
        _, norm_p = stats.shapiro(diffs)
        results["normality_p"] = norm_p
        results["use_nonparametric"] = norm_p < 0.05

    return results


def create_boxplot(
    measurements: list[Measurement],
    output_dir: str,
) -> Optional[str]:
    """Create boxplot comparing C vs T final SSIM by complexity."""
    try:
        import matplotlib
        matplotlib.use("Agg")
        import matplotlib.pyplot as plt
    except ImportError:
        print("WARNING: matplotlib not installed. Skipping visualizations.")
        return None

    c_finals = get_final_scores(measurements, "C")
    t_finals = get_final_scores(measurements, "T")

    complexities = ["simple", "medium", "complex"]
    fig, axes = plt.subplots(1, 3, figsize=(14, 5), sharey=True)

    for idx, comp in enumerate(complexities):
        c_scores = [m.ssim for m in c_finals.values() if m.complexity == comp]
        t_scores = [m.ssim for m in t_finals.values() if m.complexity == comp]

        ax = axes[idx]
        bp = ax.boxplot(
            [c_scores, t_scores],
            labels=["Control", "Treatment"],
            patch_artist=True,
            widths=0.6,
        )
        bp["boxes"][0].set_facecolor("#ff9999")
        bp["boxes"][1].set_facecolor("#99ccff")

        # Overlay individual points
        for i, scores in enumerate([c_scores, t_scores], 1):
            ax.scatter(
                [i] * len(scores), scores,
                color="black", zorder=5, s=30, alpha=0.7,
            )

        ax.set_title(f"{comp.capitalize()} (n={len(c_scores)})")
        ax.set_ylabel("Final SSIM" if idx == 0 else "")
        ax.set_ylim(0.5, 1.0)
        ax.grid(axis="y", alpha=0.3)

    fig.suptitle("Multi-Metric Similarity: Control vs Treatment (Final SSIM)")
    plt.tight_layout()

    path = os.path.join(output_dir, "boxplot_ssim.png")
    plt.savefig(path, dpi=150)
    plt.close()
    return path


def create_convergence_plot(
    measurements: list[Measurement],
    output_dir: str,
) -> Optional[str]:
    """Create convergence curves for all designs."""
    try:
        import matplotlib
        matplotlib.use("Agg")
        import matplotlib.pyplot as plt
    except ImportError:
        return None

    design_ids = sorted(set(m.design_id for m in measurements))
    n_designs = len(design_ids)
    cols = min(5, n_designs)
    rows = (n_designs + cols - 1) // cols

    fig, axes = plt.subplots(rows, cols, figsize=(4 * cols, 3.5 * rows), squeeze=False)

    for idx, did in enumerate(design_ids):
        ax = axes[idx // cols][idx % cols]

        c_ms = sorted(
            [m for m in measurements if m.design_id == did and m.group == "C"],
            key=lambda m: m.iteration,
        )
        t_ms = sorted(
            [m for m in measurements if m.design_id == did and m.group == "T"],
            key=lambda m: m.iteration,
        )

        if c_ms:
            ax.plot(
                [m.iteration for m in c_ms],
                [m.ssim for m in c_ms],
                "r-o", label="Control", markersize=4,
            )
        if t_ms:
            ax.plot(
                [m.iteration for m in t_ms],
                [m.ssim for m in t_ms],
                "b-s", label="Treatment", markersize=4,
            )

        # Early stop marker
        for m in t_ms:
            if m.stopped and m.stop_reason != "CONTINUE":
                ax.axvline(x=m.iteration, color="green", linestyle="--", alpha=0.5)
                ax.annotate(
                    m.stop_reason,
                    (m.iteration, m.ssim),
                    fontsize=6, color="green",
                )

        ax.axhline(y=0.92, color="gray", linestyle=":", alpha=0.5, label="Target 0.92")
        ax.set_title(f"{did}: {c_ms[0].design_name if c_ms else did}", fontsize=9)
        ax.set_xlabel("Iteration")
        ax.set_ylabel("SSIM")
        ax.set_ylim(0.3, 1.0)
        ax.legend(fontsize=6)
        ax.grid(alpha=0.3)

    # Hide empty subplots
    for idx in range(n_designs, rows * cols):
        axes[idx // cols][idx % cols].set_visible(False)

    fig.suptitle("Convergence Curves: Control (red) vs Treatment (blue)")
    plt.tight_layout()

    path = os.path.join(output_dir, "convergence_curves.png")
    plt.savefig(path, dpi=150)
    plt.close()
    return path


def effect_size_label(d: float) -> str:
    """Label for Cohen's d effect size."""
    ad = abs(d)
    if ad >= 0.8:
        return "large"
    elif ad >= 0.5:
        return "medium"
    elif ad >= 0.2:
        return "small"
    else:
        return "negligible"


def main() -> None:
    parser = argparse.ArgumentParser(description="Analyze proof experiment results")
    parser.add_argument(
        "--csv",
        default="test/proof_experiment_results.csv",
        help="Path to experiment CSV",
    )
    parser.add_argument(
        "--output-dir",
        default="docs",
        help="Directory for output files (plots, report)",
    )
    args = parser.parse_args()

    if not os.path.exists(args.csv):
        print(f"ERROR: CSV file not found: {args.csv}")
        print("Run proof_experiment first: dune exec test/proof_experiment.exe")
        sys.exit(1)

    os.makedirs(args.output_dir, exist_ok=True)

    # Load data
    measurements = load_csv(args.csv)
    print(f"Loaded {len(measurements)} measurements from {args.csv}")

    # Extract final scores
    c_finals = get_final_scores(measurements, "C")
    t_finals = get_final_scores(measurements, "T")

    # Align by design_id
    common_ids = sorted(set(c_finals.keys()) & set(t_finals.keys()))
    c_ssims = [c_finals[did].ssim for did in common_ids]
    t_ssims = [t_finals[did].ssim for did in common_ids]
    c_hssims = [c_finals[did].human_ssim for did in common_ids]
    t_hssims = [t_finals[did].human_ssim for did in common_ids]

    print(f"Paired designs: {len(common_ids)}")

    # Statistical tests
    results = run_statistical_tests(c_ssims, t_ssims, c_hssims, t_hssims)

    # Print results
    print("\n" + "=" * 60)
    print("  STATISTICAL ANALYSIS RESULTS")
    print("=" * 60)

    d_ssim = results.get("ssim_d", 0.0)
    d_hssim = results.get("hssim_d", 0.0)
    assert isinstance(d_ssim, (int, float))
    assert isinstance(d_hssim, (int, float))

    if "ssim_ttest_p" in results:
        p_ssim = results["ssim_ttest_p"]
        assert isinstance(p_ssim, (int, float))
        print(f"\nPaired t-test (SSIM):")
        print(f"  t = {results['ssim_ttest_t']:.4f}, p = {p_ssim:.6f}")
        print(f"  {'Significant' if p_ssim < 0.05 else 'Not significant'} at alpha=0.05")

    if "ssim_wilcoxon_p" in results:
        w_p = results["ssim_wilcoxon_p"]
        assert isinstance(w_p, (int, float))
        print(f"\nWilcoxon signed-rank (SSIM, one-sided T>C):")
        print(f"  W = {results['ssim_wilcoxon_w']:.1f}, p = {w_p:.6f}")
    elif "ssim_wilcoxon_error" in results:
        print(f"\nWilcoxon: {results['ssim_wilcoxon_error']}")

    if "normality_p" in results:
        norm_p = results["normality_p"]
        assert isinstance(norm_p, (int, float))
        print(f"\nShapiro-Wilk normality (differences): p = {norm_p:.4f}")
        print(f"  {'Non-normal' if norm_p < 0.05 else 'Normal'} distribution")
        if norm_p < 0.05:
            print("  -> Wilcoxon result preferred over t-test")

    print(f"\nEffect Sizes:")
    print(f"  SSIM Cohen's d:       {d_ssim:.4f} ({effect_size_label(d_ssim)})")
    print(f"  human_ssim Cohen's d: {d_hssim:.4f} ({effect_size_label(d_hssim)})")

    # Success criteria
    c_mean = sum(c_ssims) / len(c_ssims) if c_ssims else 0
    t_mean = sum(t_ssims) / len(t_ssims) if t_ssims else 0
    c_iter_mean = sum(c_finals[d].iteration for d in common_ids) / len(common_ids) if common_ids else 0
    t_iter_mean = sum(t_finals[d].iteration for d in common_ids) / len(common_ids) if common_ids else 0

    p_val = results.get("ssim_ttest_p", 1.0)
    assert isinstance(p_val, (int, float))

    print(f"\n{'=' * 60}")
    print("  SUCCESS CRITERIA")
    print(f"{'=' * 60}")
    print(f"  [{'PASS' if t_mean > c_mean and p_val < 0.05 else 'FAIL'}] T SSIM > C (p < 0.05): "
          f"{t_mean:.4f} vs {c_mean:.4f}, p = {p_val:.6f}")
    print(f"  [{'PASS' if abs(d_ssim) >= 0.5 else 'FAIL'}] Cohen's d >= 0.5: d = {d_ssim:.4f}")
    print(f"  [{'PASS' if t_iter_mean <= c_iter_mean else 'FAIL'}] T iterations <= C: "
          f"{t_iter_mean:.1f} vs {c_iter_mean:.1f}")

    # Generate plots
    boxplot_path = create_boxplot(measurements, args.output_dir)
    convergence_path = create_convergence_plot(measurements, args.output_dir)

    if boxplot_path:
        print(f"\nBoxplot saved to: {boxplot_path}")
    if convergence_path:
        print(f"Convergence plot saved to: {convergence_path}")

    # Write markdown report
    report_path = os.path.join(args.output_dir, "PROOF_EXPERIMENT_RESULTS.md")
    with open(report_path, "w") as f:
        f.write("# Multi-Metric Similarity Proof Experiment Results\n\n")
        f.write(f"Generated from: `{args.csv}`\n\n")
        f.write("## Summary Statistics\n\n")
        f.write("| Metric | Control (C) | Treatment (T) |\n")
        f.write("|--------|-------------|---------------|\n")
        f.write(f"| SSIM (mean) | {c_mean:.4f} | {t_mean:.4f} |\n")
        c_h_mean = sum(c_hssims) / len(c_hssims) if c_hssims else 0
        t_h_mean = sum(t_hssims) / len(t_hssims) if t_hssims else 0
        f.write(f"| human_ssim (mean) | {c_h_mean:.4f} | {t_h_mean:.4f} |\n")
        f.write(f"| Iterations (mean) | {c_iter_mean:.1f} | {t_iter_mean:.1f} |\n\n")

        f.write("## Statistical Tests\n\n")
        if "ssim_ttest_p" in results:
            f.write(f"- Paired t-test: t={results['ssim_ttest_t']:.4f}, p={p_val:.6f}\n")
        if "ssim_wilcoxon_p" in results:
            f.write(f"- Wilcoxon: W={results['ssim_wilcoxon_w']:.1f}, p={results['ssim_wilcoxon_p']:.6f}\n")
        f.write(f"- Cohen's d (SSIM): {d_ssim:.4f} ({effect_size_label(d_ssim)})\n")
        f.write(f"- Cohen's d (human_ssim): {d_hssim:.4f} ({effect_size_label(d_hssim)})\n\n")

        f.write("## Success Criteria\n\n")
        f.write(f"| Criterion | Result | Value |\n")
        f.write(f"|-----------|--------|-------|\n")
        f.write(f"| T SSIM > C (p<0.05) | {'PASS' if t_mean > c_mean and p_val < 0.05 else 'FAIL'} | {t_mean:.4f} vs {c_mean:.4f} |\n")
        f.write(f"| Cohen's d >= 0.5 | {'PASS' if abs(d_ssim) >= 0.5 else 'FAIL'} | d={d_ssim:.4f} |\n")
        f.write(f"| T iters <= C iters | {'PASS' if t_iter_mean <= c_iter_mean else 'FAIL'} | {t_iter_mean:.1f} vs {c_iter_mean:.1f} |\n")

        if boxplot_path:
            f.write(f"\n## Visualizations\n\n")
            f.write(f"![Boxplot]({os.path.basename(boxplot_path)})\n\n")
        if convergence_path:
            f.write(f"![Convergence]({os.path.basename(convergence_path)})\n")

    print(f"Report saved to: {report_path}")
    print("=" * 60)


if __name__ == "__main__":
    main()
