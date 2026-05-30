"""
01_download_OISST.py
--------------------
Downloads NOAA OISST v2.1 daily SST data for the Mafia Island MPA bounding box.

Output: data/raw/OISST_MIMP_1985_2024.nc

Usage:
    python scripts/01_download_OISST.py

Notes:
    - Uses NOAA THREDDS OPeNDAP server for spatial subsetting at source
    - Downloads only the MIMP bounding box — avoids downloading global files
    - If the server is unavailable, fallback instructions are printed
    - Estimated download size: ~50–80 MB for the full 1985–2024 record
"""

import os
import time
import requests
from pathlib import Path

# ─── Paths ──────────────────────────────────────────────────────────────────
SCRIPT_DIR = Path(__file__).parent
RAW_DIR    = SCRIPT_DIR.parent / "data" / "raw"
RAW_DIR.mkdir(parents=True, exist_ok=True)

OUTPUT_FILE = RAW_DIR / "OISST_MIMP_1985_2024.nc"

# ─── MIMP Bounding Box ───────────────────────────────────────────────────────
# Mafia Island MPA: centre ~(-7.85, 39.82); box adds 0.5° buffer
LAT_MIN = -8.5
LAT_MAX = -7.0
LON_MIN = 39.0
LON_MAX = 40.5

# ─── OISST THREDDS OPeNDAP URL template ─────────────────────────────────────
# NOAA OISST v2.1 daily files at NCEI THREDDS
# Full catalog: https://www.ncei.noaa.gov/thredds/catalog/OisstBase/NetCDF/V2.1/AVHRR/catalog.html
THREDDS_BASE = (
    "https://www.ncei.noaa.gov/thredds/dodsC/OisstBase/NetCDF/V2.1/AVHRR/"
)

def download_via_opendap():
    """
    Download OISST using xarray + OPeNDAP for direct spatial subsetting.
    Requires: xarray, netCDF4, numpy
    """
    try:
        import xarray as xr
        import numpy as np
    except ImportError:
        print("ERROR: xarray or netCDF4 not installed.")
        print("Install with: pip install xarray netCDF4 scipy")
        return False

    print("Connecting to NOAA THREDDS via OPeNDAP...")
    print(f"  Bounding box: Lat {LAT_MIN}–{LAT_MAX}, Lon {LON_MIN}–{LON_MAX}")
    print(f"  Period: 1985–2024")
    print()

    # OISST v2.1 is stored as monthly files on THREDDS
    # We iterate year-month and concatenate
    datasets = []
    failed  = []

    for year in range(1985, 2025):
        for month in range(1, 13):
            url = f"{THREDDS_BASE}{year}{month:02d}/oisst-avhrr-v02r01.{year}{month:02d}01.nc"
            try:
                ds = xr.open_dataset(url, engine="netcdf4")
                # Subset to MIMP box
                ds_sub = ds.sel(
                    lat=slice(LAT_MIN, LAT_MAX),
                    lon=slice(LON_MIN, LON_MAX)
                )
                # Spatial mean — single time series for MIMP
                ds_mean = ds_sub["sst"].mean(dim=["lat", "lon"])
                datasets.append(ds_mean)
                print(f"  ✓ {year}-{month:02d}")
                time.sleep(0.3)   # be polite to the server
            except Exception as e:
                print(f"  ✗ {year}-{month:02d}: {e}")
                failed.append(f"{year}-{month:02d}")

    if not datasets:
        print("\nNo data downloaded. Check connection or URL structure.")
        return False

    print("\nConcatenating and saving...")
    combined = xr.concat(datasets, dim="time")
    combined.to_netcdf(OUTPUT_FILE)
    print(f"\nSaved: {OUTPUT_FILE}")

    if failed:
        print(f"\nFailed months ({len(failed)}): {', '.join(failed)}")
        print("Re-run the script or download these manually.")

    return True


def print_manual_instructions():
    """Print manual download instructions as fallback."""
    print("\n" + "="*60)
    print("MANUAL DOWNLOAD INSTRUCTIONS")
    print("="*60)
    print()
    print("1. Go to: https://www.ncei.noaa.gov/products/optimum-interpolation-sst")
    print()
    print("2. Use the data access portal to select:")
    print("   - Product: OISST v2.1")
    print("   - Temporal: 1985-01-01 to 2024-12-31")
    print(f"   - Spatial: Lat {LAT_MIN} to {LAT_MAX}, Lon {LON_MIN} to {LON_MAX}")
    print("   - Variable: sea_surface_temperature (sst)")
    print()
    print("3. Or use the OPeNDAP THREDDS catalog:")
    print(f"   {THREDDS_BASE}")
    print()
    print("4. Save the file as:")
    print(f"   {OUTPUT_FILE}")
    print()
    print("Alternative: ERDDAP subset tool")
    print("   https://coastwatch.pfeg.noaa.gov/erddap/griddap/ncdcOisst21Agg_LonPM180.html")
    print("   Variable: sst | Same lat/lon/time bounds as above")


if __name__ == "__main__":
    if OUTPUT_FILE.exists():
        print(f"Output file already exists: {OUTPUT_FILE}")
        print("Delete it to re-download, or proceed to script 03_process_SST.py")
    else:
        success = download_via_opendap()
        if not success:
            print_manual_instructions()
