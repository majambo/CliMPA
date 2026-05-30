"""
09_fig5_heatmap.py
------------------
Figure 5 — SST Anomaly Heatmap (Hovmöller-style)

Year × Month grid coloured by SST anomaly vs 1985–2005 baseline.
Blue = cooler than average, Red = warmer than average.

Inputs:
    data/processed/SST_anomaly_monthly.csv

Outputs:
    figures/fig5_SST_anomaly_heatmap.png
    figures/fig5_SST_anomaly_heatmap.svg

Usage:
    python scripts/09_fig5_heatmap.py
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
from pathlib import Path

# ─── Paths ───────────────────────────────────────────────────────────────────
SCRIPT_DIR = Path(__file__).parent
DATA_PROC  = SCRIPT_DIR.parent / "data" / "processed"
FIG_DIR    = SCRIPT_DIR.parent / "figures"
FIG_DIR.mkdir(parents=True, exist_ok=True)

BLEACHING_YEARS = [1998, 2010, 2016, 2024]
BASELINE_END    = 2005

MONTH_LABELS = ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]


def main():
    # ── 1. Load anomaly data ──────────────────────────────────────────────
    anomaly = pd.read_csv(DATA_PROC / "SST_anomaly_monthly.csv")
    anomaly = anomaly.dropna(subset=["anomaly"])

    # Pivot to year × month grid
    grid = anomaly.pivot(index="year", columns="month", values="anomaly")
    grid = grid.sort_index(ascending=True)   # oldest year at top

    years  = grid.index.values
    n_years = len(years)

    # ── 2. Set up colour scale ────────────────────────────────────────────
    # Symmetric around zero; clip at ±1.5°C
    vmax = 1.5
    cmap = plt.cm.RdBu_r   # Blue (cool) → White (neutral) → Red (warm)
    norm = mcolors.TwoSlopeNorm(vmin=-vmax, vcenter=0, vmax=vmax)

    # ── 3. Plot ───────────────────────────────────────────────────────────
    fig, ax = plt.subplots(figsize=(12, n_years * 0.28 + 2))

    im = ax.imshow(
        grid.values,
        aspect="auto",
        cmap=cmap,
        norm=norm,
        interpolation="nearest"
    )

    # Baseline separator line
    baseline_end_idx = np.where(years == BASELINE_END)[0]
    if len(baseline_end_idx) > 0:
        ax.axhline(baseline_end_idx[0] + 0.5, color="#212121", linewidth=1.5, linestyle="-")
        ax.text(-0.8, baseline_end_idx[0] + 0.5, "Baseline\nend",
                fontsize=7, ha="right", va="center", color="#424242")

    # Outline bleaching years
    for byear in BLEACHING_YEARS:
        yr_idx = np.where(years == byear)[0]
        if len(yr_idx) > 0:
            rect = plt.Rectangle(
                (-0.5, yr_idx[0] - 0.5), 12, 1,
                linewidth=2, edgecolor="#212121", facecolor="none"
            )
            ax.add_patch(rect)
            ax.text(12.7, yr_idx[0], str(byear),
                    fontsize=7.5, ha="left", va="center", color="#D32F2F",
                    fontweight="bold")

    # Axes formatting
    ax.set_xticks(range(12))
    ax.set_xticklabels(MONTH_LABELS, fontsize=9)
    ax.set_yticks(range(n_years))
    ax.set_yticklabels(years, fontsize=7)
    ax.set_xlabel("Month", fontsize=11)
    ax.set_ylabel("Year", fontsize=11)
    ax.set_title("Monthly SST Anomaly — Mafia Island MPA (1985–2024)",
                 fontsize=13, fontweight="bold", pad=12)

    # Colour bar
    cbar = plt.colorbar(im, ax=ax, shrink=0.6, pad=0.01)
    cbar.set_label("SST anomaly (°C)\nvs 1985–2005 baseline", fontsize=9)
    cbar.ax.tick_params(labelsize=8)

    # Caption
    fig.text(0.01, -0.02,
             "Figure 5.  Monthly SST anomaly heatmap at Mafia Island MPA (1985–2024). "
             "Each cell shows departure from the 1985–2005 climatological monthly mean. "
             "Blue = cooler than average, red = warmer. Bleaching years outlined in black.  "
             "Source: NOAA OISST v2.1.",
             fontsize=8, color="#616161", style="italic", wrap=True)

    plt.tight_layout(rect=[0, 0.03, 1, 1])

    # ── 4. Save ───────────────────────────────────────────────────────────
    png_path = FIG_DIR / "fig5_SST_anomaly_heatmap.png"
    svg_path = FIG_DIR / "fig5_SST_anomaly_heatmap.svg"
    plt.savefig(png_path, dpi=300, bbox_inches="tight")
    plt.savefig(svg_path, bbox_inches="tight")
    print(f"  Saved: {png_path}")
    print(f"  Saved: {svg_path}")
    plt.close()

    # ── 5. Quick stats ────────────────────────────────────────────────────
    recent_decade = anomaly[anomaly["year"] >= 2015]["anomaly"]
    first_decade  = anomaly[anomaly["year"] <= 1995]["anomaly"]
    print(f"\n  Mean anomaly 1985–1995: {first_decade.mean():+.2f} °C")
    print(f"  Mean anomaly 2015–2024: {recent_decade.mean():+.2f} °C")
    print(f"  Shift: {recent_decade.mean() - first_decade.mean():+.2f} °C")


if __name__ == "__main__":
    main()
