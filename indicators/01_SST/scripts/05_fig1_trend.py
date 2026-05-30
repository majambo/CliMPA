"""
05_fig1_trend.py
----------------
Figure 1 — Long-Term SST Trend at Mafia Island MPA

Produces a monthly SST time series with:
  - Linear trend line (and slope annotation)
  - LOESS smoothed line
  - Bleaching event annotations (1998, 2010, 2016, 2024)
  - Optional WIO comparison line

Inputs:
    data/processed/SST_monthly.csv

Outputs:
    figures/fig1_SST_longterm_trend.png
    figures/fig1_SST_longterm_trend.svg
    data/outputs/fig1_key_numbers.json  ← warming rate, etc.

Usage:
    python scripts/05_fig1_trend.py
"""

import json
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from pathlib import Path
from scipy import stats
from scipy.ndimage import uniform_filter1d

# ─── Paths ───────────────────────────────────────────────────────────────────
SCRIPT_DIR  = Path(__file__).parent
DATA_PROC   = SCRIPT_DIR.parent / "data" / "processed"
DATA_OUT    = SCRIPT_DIR.parent / "data" / "outputs"
FIG_DIR     = SCRIPT_DIR.parent / "figures"
DATA_OUT.mkdir(parents=True, exist_ok=True)
FIG_DIR.mkdir(parents=True, exist_ok=True)

# ─── Style (from style_guide.md) ─────────────────────────────────────────────
COLOUR_SST    = "#1565C0"   # Deep Blue — SST line
COLOUR_TREND  = "#212121"   # Near-black — trend line
COLOUR_LOESS  = "#D32F2F"   # Red — smoothed/LOESS
COLOUR_ANNOT  = "#9E9E9E"   # Light grey — bleaching event lines

BLEACHING_YEARS = {
    1998: "1998\nEl Niño",
    2010: "2010",
    2016: "2016\nEl Niño",
    2024: "2024",
}


def loess_smooth(x, y, window=24):
    """Simple moving-average smoothing as LOESS approximation."""
    return uniform_filter1d(y.astype(float), size=window)


def main():
    # ── 1. Load data ──────────────────────────────────────────────────────
    monthly = pd.read_csv(DATA_PROC / "SST_monthly.csv", parse_dates=["date"])
    monthly = monthly.sort_values("date").reset_index(drop=True)

    x = np.arange(len(monthly))   # integer index for regression
    y = monthly["sst_mean"].values

    # ── 2. Linear trend ───────────────────────────────────────────────────
    slope, intercept, r_value, p_value, std_err = stats.linregress(x, y)
    trend_line = slope * x + intercept

    # Convert slope from °C/month to °C/decade
    slope_per_decade = slope * 12 * 10
    print(f"Linear trend: {slope_per_decade:+.3f} °C/decade  (p={p_value:.4f})")

    # ── 3. LOESS / smoothed line ──────────────────────────────────────────
    smoothed = loess_smooth(x, y, window=24)

    # ── 4. Plot ───────────────────────────────────────────────────────────
    fig, ax = plt.subplots(figsize=(13, 5.5))

    # Raw monthly SST (thin, semi-transparent)
    ax.plot(monthly["date"], y, color=COLOUR_SST, linewidth=0.7, alpha=0.6, label="Monthly SST")

    # Smoothed line
    ax.plot(monthly["date"], smoothed, color=COLOUR_LOESS, linewidth=1.8,
            alpha=0.85, label="24-month smoothed")

    # Linear trend
    ax.plot(monthly["date"], trend_line, color=COLOUR_TREND, linewidth=1.5,
            linestyle="--", label=f"Linear trend ({slope_per_decade:+.2f} °C/decade)")

    # Bleaching event annotations
    for year, label in BLEACHING_YEARS.items():
        event_date = pd.Timestamp(f"{year}-03-01")  # typically Feb-Apr peak
        if event_date >= monthly["date"].min() and event_date <= monthly["date"].max():
            ax.axvline(event_date, color=COLOUR_ANNOT, linewidth=1.0, linestyle=":", alpha=0.9)
            ax.text(event_date, ax.get_ylim()[1] if ax.get_ylim()[1] else y.max() + 0.3,
                    label, fontsize=7.5, ha="center", va="bottom",
                    color="#616161", rotation=0)

    # ── 5. Formatting ─────────────────────────────────────────────────────
    ax.set_title("Sea Surface Temperature — Mafia Island MPA (1985–2024)",
                 fontsize=13, fontweight="bold", pad=12)
    ax.set_xlabel("Year", fontsize=11)
    ax.set_ylabel("SST (°C)", fontsize=11)
    ax.legend(loc="upper left", fontsize=9, framealpha=0.9)
    ax.tick_params(labelsize=9)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)

    # Annotate warming rate prominently
    mid_date = monthly["date"].iloc[len(monthly)//2]
    ax.annotate(
        f"Warming rate: {slope_per_decade:+.2f} °C/decade",
        xy=(mid_date, trend_line[len(trend_line)//2]),
        xytext=(0, 18), textcoords="offset points",
        fontsize=9, color=COLOUR_TREND,
        arrowprops=dict(arrowstyle="->", color=COLOUR_TREND, lw=0.8)
    )

    # Caption
    fig.text(0.01, -0.04,
             "Figure 1.  Monthly SST at Mafia Island MPA (1985–2024) with linear warming trend "
             "and annotated bleaching events.  Source: NOAA OISST v2.1.",
             fontsize=8, color="#616161", style="italic")

    plt.tight_layout(rect=[0, 0.02, 1, 1])

    # ── 6. Save figures ───────────────────────────────────────────────────
    png_path = FIG_DIR / "fig1_SST_longterm_trend.png"
    svg_path = FIG_DIR / "fig1_SST_longterm_trend.svg"
    plt.savefig(png_path, dpi=300, bbox_inches="tight")
    plt.savefig(svg_path, bbox_inches="tight")
    print(f"\n  Saved: {png_path}")
    print(f"  Saved: {svg_path}")
    plt.close()

    # ── 7. Export key numbers ─────────────────────────────────────────────
    key_numbers = {
        "figure": "Fig1 — Long-Term SST Trend",
        "warming_rate_per_decade_C": round(float(slope_per_decade), 3),
        "trend_p_value": round(float(p_value), 4),
        "trend_r_squared": round(float(r_value**2), 3),
        "mean_sst_baseline_1985_2005": round(float(
            monthly[monthly["year"].between(1985, 2005)]["sst_mean"].mean()
        ), 2),
        "mean_sst_recent_2010_2024": round(float(
            monthly[monthly["year"].between(2010, 2024)]["sst_mean"].mean()
        ), 2),
    }
    out_json = DATA_OUT / "fig1_key_numbers.json"
    with open(out_json, "w") as f:
        json.dump(key_numbers, f, indent=2)
    print(f"  Saved: {out_json}")
    print(f"\n  HEADLINE: Warming rate = {slope_per_decade:+.3f} °C/decade")


if __name__ == "__main__":
    main()
