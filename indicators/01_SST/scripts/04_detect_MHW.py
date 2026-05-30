"""
04_detect_MHW.py
----------------
Detects Marine Heatwave (MHW) events at MIMP using the Hobday et al. (2016/2018)
framework and classifies each event by intensity category.

Inputs:
    data/processed/SST_daily.csv
    data/processed/SST_climatology.csv

Outputs:
    data/processed/MHW_daily_flags.csv   — daily SST with MHW flag and category
    data/processed/MHW_events.csv        — one row per MHW event (start, end, stats)
    data/processed/MHW_annual_stats.csv  — annual aggregates (days, intensity, duration)

Usage:
    python scripts/04_detect_MHW.py

References:
    Hobday et al. (2016) Progress in Oceanography 141:227–238
    Hobday et al. (2018) Oceanography 31(2):162–173
"""

import numpy as np
import pandas as pd
from pathlib import Path

# ─── Paths ───────────────────────────────────────────────────────────────────
SCRIPT_DIR = Path(__file__).parent
DATA_PROC  = SCRIPT_DIR.parent / "data" / "processed"

# ─── MHW Category thresholds (Hobday et al. 2018) ────────────────────────────
# Category is based on multiples of the gap between P90 and clim_mean
# gap = P90 - clim_mean
# Moderate:  SST ≥ P90 + 0×gap  (i.e. above P90)
# Strong:    SST ≥ P90 + 1×gap
# Severe:    SST ≥ P90 + 2×gap
# Extreme:   SST ≥ P90 + 3×gap

MHW_MIN_DURATION = 5   # consecutive days above P90 to qualify as MHW


def classify_category(intensity, gap):
    """
    Classify MHW intensity category.
    intensity = SST - P90 (must be > 0 to be a heatwave day)
    gap = P90 - clim_mean
    """
    if intensity <= 0 or gap <= 0:
        return None
    multiples = intensity / gap
    if multiples >= 3:
        return "Extreme"
    elif multiples >= 2:
        return "Severe"
    elif multiples >= 1:
        return "Strong"
    else:
        return "Moderate"


def detect_mhw_events(daily_df):
    """
    Detect MHW events: periods where SST > P90 for ≥ MHW_MIN_DURATION consecutive days.
    Returns a DataFrame with one row per event.
    """
    events = []
    in_event = False
    event_start = None
    event_days = []

    for _, row in daily_df.iterrows():
        is_above = row["sst"] > row["p90"]

        if is_above:
            if not in_event:
                in_event = True
                event_start = row["date"]
                event_days = []
            event_days.append(row)
        else:
            if in_event:
                if len(event_days) >= MHW_MIN_DURATION:
                    event_df = pd.DataFrame(event_days)
                    events.append({
                        "event_id":      len(events) + 1,
                        "start_date":    event_df["date"].min(),
                        "end_date":      event_df["date"].max(),
                        "duration_days": len(event_df),
                        "max_intensity": event_df["intensity"].max(),
                        "mean_intensity":event_df["intensity"].mean(),
                        "max_category":  event_df["category"].dropna().mode().iloc[0] if not event_df["category"].dropna().empty else None,
                        "peak_date":     event_df.loc[event_df["intensity"].idxmax(), "date"],
                        "peak_sst":      event_df["sst"].max(),
                    })
                in_event = False
                event_days = []

    # Close any open event at end of record
    if in_event and len(event_days) >= MHW_MIN_DURATION:
        event_df = pd.DataFrame(event_days)
        events.append({
            "event_id":      len(events) + 1,
            "start_date":    event_df["date"].min(),
            "end_date":      event_df["date"].max(),
            "duration_days": len(event_df),
            "max_intensity": event_df["intensity"].max(),
            "mean_intensity":event_df["intensity"].mean(),
            "max_category":  event_df["category"].dropna().mode().iloc[0] if not event_df["category"].dropna().empty else None,
            "peak_date":     event_df.loc[event_df["intensity"].idxmax(), "date"],
            "peak_sst":      event_df["sst"].max(),
        })

    return pd.DataFrame(events)


def main():
    # ── 1. Load processed data ────────────────────────────────────────────
    daily = pd.read_csv(DATA_PROC / "SST_daily.csv", parse_dates=["date"])
    clim  = pd.read_csv(DATA_PROC / "SST_climatology.csv")

    print(f"Loaded daily SST: {len(daily)} records")
    print(f"Loaded climatology: {len(clim)} calendar days")

    # ── 2. Merge climatology onto daily ───────────────────────────────────
    daily = daily.merge(clim[["doy", "clim_mean", "p90"]], on="doy", how="left")

    # ── 3. Compute intensity and gap ──────────────────────────────────────
    daily["gap"]       = daily["p90"] - daily["clim_mean"]
    daily["intensity"] = np.where(
        daily["sst"] > daily["p90"],
        daily["sst"] - daily["p90"],
        0.0
    )
    daily["is_mhw"]    = daily["sst"] > daily["p90"]

    # ── 4. Classify categories ────────────────────────────────────────────
    daily["category"] = daily.apply(
        lambda r: classify_category(r["intensity"], r["gap"]) if r["is_mhw"] else None,
        axis=1
    )

    # ── 5. Mark confirmed MHW events (≥5 consecutive days) ───────────────
    # We need to flag days that are part of a qualifying event
    # (not just any day above P90)
    daily["mhw_event"] = False
    in_streak = False
    streak_indices = []

    for idx, row in daily.iterrows():
        if row["is_mhw"]:
            streak_indices.append(idx)
        else:
            if len(streak_indices) >= MHW_MIN_DURATION:
                daily.loc[streak_indices, "mhw_event"] = True
            streak_indices = []

    if len(streak_indices) >= MHW_MIN_DURATION:
        daily.loc[streak_indices, "mhw_event"] = True

    # Zero intensity on non-event days
    daily.loc[~daily["mhw_event"], "intensity"] = 0.0
    daily.loc[~daily["mhw_event"], "category"]  = None

    # ── 6. Save daily flags ───────────────────────────────────────────────
    out_daily = DATA_PROC / "MHW_daily_flags.csv"
    daily.to_csv(out_daily, index=False)
    print(f"\n  Saved: {out_daily}")

    # ── 7. Detect and save events ─────────────────────────────────────────
    events = detect_mhw_events(daily)
    out_events = DATA_PROC / "MHW_events.csv"
    events.to_csv(out_events, index=False)
    print(f"  Saved: {out_events} ({len(events)} events detected)")

    # ── 8. Annual aggregates ──────────────────────────────────────────────
    annual = (
        daily[daily["mhw_event"]]
        .groupby("year")
        .agg(
            mhw_days    = ("mhw_event", "sum"),
            mean_intensity = ("intensity", "mean"),
        )
        .reset_index()
    )
    # Add years with zero MHW days
    all_years = pd.DataFrame({"year": range(daily["year"].min(), daily["year"].max() + 1)})
    annual = all_years.merge(annual, on="year", how="left").fillna(0)

    # Mean duration per year (from events table)
    events["year"] = pd.to_datetime(events["start_date"]).dt.year
    dur_by_year = events.groupby("year")["duration_days"].mean().rename("mean_duration")
    annual = annual.merge(dur_by_year, on="year", how="left").fillna(0)

    out_annual = DATA_PROC / "MHW_annual_stats.csv"
    annual.to_csv(out_annual, index=False)
    print(f"  Saved: {out_annual}")

    # ── 9. Summary ────────────────────────────────────────────────────────
    total_mhw_days = daily["mhw_event"].sum()
    cat_counts = daily[daily["mhw_event"]]["category"].value_counts()

    print("\n" + "="*50)
    print("MHW DETECTION SUMMARY")
    print("="*50)
    print(f"  Total MHW events detected: {len(events)}")
    print(f"  Total MHW days: {total_mhw_days} ({total_mhw_days/len(daily)*100:.1f}% of record)")
    print(f"  Category breakdown:")
    for cat in ["Moderate", "Strong", "Severe", "Extreme"]:
        n = cat_counts.get(cat, 0)
        print(f"    {cat:10s}: {n:5d} days")
    if len(events) > 0:
        print(f"\n  Longest event: {events['duration_days'].max():.0f} days")
        print(f"  Most intense:  {events['max_intensity'].max():.2f} °C above P90")
        print(f"  Most recent:   {events['end_date'].max()}")


if __name__ == "__main__":
    main()
