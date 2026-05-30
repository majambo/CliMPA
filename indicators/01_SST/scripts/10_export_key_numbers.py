"""
10_export_key_numbers.py
------------------------
Collects all headline numbers from SST indicator outputs into a single
summary JSON file for use in the fact sheet narrative.

Inputs:
    data/outputs/fig1_key_numbers.json
    data/processed/MHW_annual_stats.csv
    data/processed/MHW_events.csv
    data/processed/SST_climatology.csv
    data/processed/SST_monthly.csv

Outputs:
    data/outputs/SST_headline_numbers.json

Usage:
    python scripts/10_export_key_numbers.py
"""

import json
import numpy as np
import pandas as pd
from pathlib import Path

SCRIPT_DIR = Path(__file__).parent
DATA_PROC  = SCRIPT_DIR.parent / "data" / "processed"
DATA_OUT   = SCRIPT_DIR.parent / "data" / "outputs"
DATA_OUT.mkdir(parents=True, exist_ok=True)


def safe_load_json(path):
    try:
        with open(path) as f:
            return json.load(f)
    except FileNotFoundError:
        print(f"  MISSING: {path.name} — run corresponding figure script first")
        return {}


def main():
    numbers = {}

    # ── Fig 1: Warming trend ──────────────────────────────────────────────
    fig1 = safe_load_json(DATA_OUT / "fig1_key_numbers.json")
    numbers["warming_rate_C_per_decade"] = fig1.get("warming_rate_per_decade_C")
    numbers["trend_significant"]         = (fig1.get("trend_p_value", 1) < 0.05)
    numbers["mean_sst_baseline"]         = fig1.get("mean_sst_baseline_1985_2005")
    numbers["mean_sst_recent"]           = fig1.get("mean_sst_recent_2010_2024")

    # ── Fig 2: Seasonal shift ─────────────────────────────────────────────
    try:
        monthly = pd.read_csv(DATA_PROC / "SST_monthly.csv")

        early  = monthly[monthly["year"].between(1985, 2000)]
        recent = monthly[monthly["year"].between(2005, 2024)]

        early_cool  = early[early["month"].isin([6,7,8,9])]["sst_mean"].mean()
        recent_cool = recent[recent["month"].isin([6,7,8,9])]["sst_mean"].mean()
        numbers["cool_season_shift_C"] = round(float(recent_cool - early_cool), 2)
        numbers["cool_season_mean_early_1985_2000"] = round(float(early_cool), 2)
        numbers["cool_season_mean_recent_2005_2024"] = round(float(recent_cool), 2)
    except Exception as e:
        print(f"  Fig 2 numbers: {e}")

    # ── Fig 3: MHW stats ──────────────────────────────────────────────────
    try:
        mhw_annual = pd.read_csv(DATA_PROC / "MHW_annual_stats.csv")
        mhw_events = pd.read_csv(DATA_PROC / "MHW_events.csv", parse_dates=["start_date", "end_date"])

        decade1 = mhw_annual[mhw_annual["year"].between(1985, 1994)]
        decade4 = mhw_annual[mhw_annual["year"].between(2015, 2024)]

        numbers["mhw_days_per_year_1985_1994"] = round(float(decade1["mhw_days"].mean()), 1)
        numbers["mhw_days_per_year_2015_2024"] = round(float(decade4["mhw_days"].mean()), 1)
        numbers["mhw_total_events"]            = int(len(mhw_events))
        numbers["mhw_longest_event_days"]      = int(mhw_events["duration_days"].max()) if len(mhw_events) > 0 else 0
        numbers["mhw_most_intense_C_above_p90"] = round(float(mhw_events["max_intensity"].max()), 2) if len(mhw_events) > 0 else 0
    except Exception as e:
        print(f"  Fig 3 numbers: {e}")

    # ── Fig 4: DHW stats ──────────────────────────────────────────────────
    # DHW numbers come from separate CRW data; placeholder here
    numbers["dhw_note"] = "DHW numbers to be populated from CRW data (script 08_fig4_DHW.py)"

    # ── Output ────────────────────────────────────────────────────────────
    out_path = DATA_OUT / "SST_headline_numbers.json"
    with open(out_path, "w") as f:
        json.dump(numbers, f, indent=2)

    print("\n" + "="*55)
    print("SST HEADLINE NUMBERS")
    print("="*55)
    for k, v in numbers.items():
        print(f"  {k}: {v}")
    print(f"\n  Saved: {out_path}")


if __name__ == "__main__":
    main()
