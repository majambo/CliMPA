"""
03_process_SST.py
-----------------
Processes raw NOAA OISST v2.1 NetCDF into clean analysis-ready CSV files.

Inputs:
    data/raw/OISST_MIMP_1985_2024.nc

Outputs:
    data/processed/SST_daily.csv          — daily SST time series
    data/processed/SST_monthly.csv        — monthly means
    data/processed/SST_annual.csv         — annual means
    data/processed/SST_climatology.csv    — daily climatology (clim_mean + P90)
    data/processed/SST_anomaly_monthly.csv — monthly anomalies vs 1985–2005 baseline

Usage:
    python scripts/03_process_SST.py
"""

import numpy as np
import pandas as pd
from pathlib import Path

# ─── Paths ───────────────────────────────────────────────────────────────────
SCRIPT_DIR  = Path(__file__).parent
DATA_RAW    = SCRIPT_DIR.parent / "data" / "raw"
DATA_PROC   = SCRIPT_DIR.parent / "data" / "processed"
DATA_PROC.mkdir(parents=True, exist_ok=True)

OISST_FILE  = DATA_RAW / "OISST_MIMP_1985_2024.nc"

# ─── Parameters ──────────────────────────────────────────────────────────────
BASELINE_START = 1985
BASELINE_END   = 2005
RECORD_START   = 1985
RECORD_END     = 2024
CENTRE_LAT     = -7.85
CENTRE_LON     = 39.82

# Hobday et al. (2016): ±15 day window for climatology
CLIM_WINDOW = 15


def load_oisst(filepath):
    """Load OISST NetCDF and extract nearest-point daily SST time series."""
    try:
        import xarray as xr
    except ImportError:
        raise ImportError("Install xarray: pip install xarray netCDF4")

    print(f"Loading: {filepath}")
    ds = xr.open_dataset(filepath)

    # If the file is already a spatial mean (from download script), use directly
    # If not, select nearest grid cell to MIMP centre
    if "lat" in ds.dims and "lon" in ds.dims:
        print(f"  Selecting nearest grid cell to ({CENTRE_LAT}, {CENTRE_LON})")
        ds = ds.sel(lat=CENTRE_LAT, lon=CENTRE_LON, method="nearest")

    # Extract SST variable (handle common naming conventions)
    sst_var = None
    for name in ["sst", "SST", "sea_surface_temperature", "analysed_sst"]:
        if name in ds:
            sst_var = name
            break
    if sst_var is None:
        print(f"  Available variables: {list(ds.data_vars)}")
        raise KeyError("SST variable not found. Check variable names above.")

    sst = ds[sst_var].to_dataframe().reset_index()
    sst = sst.rename(columns={sst_var: "sst", "time": "date"})
    sst["date"] = pd.to_datetime(sst["date"])
    sst = sst[["date", "sst"]].sort_values("date").reset_index(drop=True)

    # OISST stores temperature in Kelvin in some versions — convert if needed
    if sst["sst"].mean() > 200:
        print("  Converting from Kelvin to Celsius")
        sst["sst"] = sst["sst"] - 273.15

    print(f"  Loaded {len(sst)} daily records ({sst['date'].min().date()} to {sst['date'].max().date()})")
    return sst


def compute_climatology(daily_df):
    """
    Compute daily climatological mean and P90 threshold.
    Uses ±15 day window across baseline years (Hobday et al. 2016).
    Returns one row per calendar day (1–366).
    """
    print("Computing climatology (Hobday et al. 2016 method)...")
    baseline = daily_df[
        daily_df["date"].dt.year.between(BASELINE_START, BASELINE_END)
    ].copy()
    baseline["doy"] = baseline["date"].dt.day_of_year

    results = []
    for doy in range(1, 367):
        # Build ±15 day window (circular, wrapping over year boundary)
        window_doys = set()
        for d in range(doy - CLIM_WINDOW, doy + CLIM_WINDOW + 1):
            # Wrap to 1–365
            d_wrap = ((d - 1) % 365) + 1
            window_doys.add(d_wrap)

        window_vals = baseline[baseline["doy"].isin(window_doys)]["sst"]

        if len(window_vals) < 30:
            # Not enough data — skip or NaN
            results.append({"doy": doy, "clim_mean": np.nan, "p90": np.nan, "n_obs": len(window_vals)})
        else:
            results.append({
                "doy":       doy,
                "clim_mean": window_vals.mean(),
                "p90":       window_vals.quantile(0.90),
                "n_obs":     len(window_vals)
            })

    clim_df = pd.DataFrame(results)
    print(f"  Climatology computed: {len(clim_df)} calendar days, avg {clim_df['n_obs'].mean():.0f} obs/day")
    return clim_df


def compute_anomalies(monthly_df, baseline_monthly):
    """Compute monthly SST anomalies against 1985–2005 monthly means."""
    baseline_means = (
        baseline_monthly[baseline_monthly["year"].between(BASELINE_START, BASELINE_END)]
        .groupby("month")["sst_mean"]
        .mean()
        .rename("clim_monthly_mean")
    )
    anomaly_df = monthly_df.copy()
    anomaly_df = anomaly_df.join(baseline_means, on="month")
    anomaly_df["anomaly"] = anomaly_df["sst_mean"] - anomaly_df["clim_monthly_mean"]
    return anomaly_df


def main():
    # ── 1. Load raw data ──────────────────────────────────────────────────
    if not OISST_FILE.exists():
        print(f"ERROR: Raw data file not found: {OISST_FILE}")
        print("Run script 01_download_OISST.py first.")
        return

    daily = load_oisst(OISST_FILE)

    # ── 2. Daily CSV ──────────────────────────────────────────────────────
    daily["year"]  = daily["date"].dt.year
    daily["month"] = daily["date"].dt.month
    daily["doy"]   = daily["date"].dt.day_of_year
    out_daily = DATA_PROC / "SST_daily.csv"
    daily.to_csv(out_daily, index=False)
    print(f"  Saved: {out_daily}")

    # ── 3. Monthly means ──────────────────────────────────────────────────
    monthly = (
        daily.groupby(["year", "month"])["sst"]
        .agg(sst_mean="mean", sst_min="min", sst_max="max", n_days="count")
        .reset_index()
    )
    monthly["date"] = pd.to_datetime(monthly[["year", "month"]].assign(day=15))
    out_monthly = DATA_PROC / "SST_monthly.csv"
    monthly.to_csv(out_monthly, index=False)
    print(f"  Saved: {out_monthly}")

    # ── 4. Annual means ───────────────────────────────────────────────────
    annual = (
        daily.groupby("year")["sst"]
        .agg(sst_mean="mean", sst_min="min", sst_max="max")
        .reset_index()
    )
    out_annual = DATA_PROC / "SST_annual.csv"
    annual.to_csv(out_annual, index=False)
    print(f"  Saved: {out_annual}")

    # ── 5. Daily climatology (clim_mean + P90) ────────────────────────────
    clim = compute_climatology(daily)
    out_clim = DATA_PROC / "SST_climatology.csv"
    clim.to_csv(out_clim, index=False)
    print(f"  Saved: {out_clim}")

    # ── 6. Monthly anomalies ──────────────────────────────────────────────
    anomaly = compute_anomalies(monthly, monthly)
    out_anom = DATA_PROC / "SST_anomaly_monthly.csv"
    anomaly.to_csv(out_anom, index=False)
    print(f"  Saved: {out_anom}")

    # ── 7. Quick summary ─────────────────────────────────────────────────
    print("\n" + "="*50)
    print("PROCESSING SUMMARY")
    print("="*50)
    print(f"  Record: {daily['date'].min().date()} to {daily['date'].max().date()}")
    print(f"  N daily records: {len(daily)}")
    print(f"  SST range: {daily['sst'].min():.2f} – {daily['sst'].max():.2f} °C")
    print(f"  Annual mean range: {annual['sst_mean'].min():.2f} – {annual['sst_mean'].max():.2f} °C")
    print(f"\n  All outputs saved to: {DATA_PROC}")


if __name__ == "__main__":
    main()
