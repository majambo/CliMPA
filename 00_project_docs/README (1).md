# Indicator 01 — Sea Surface Temperature (SST)

**Fact Sheet Section:** Climate Indicator #1  
**Data source:** NOAA OISST v2.1 + NOAA Coral Reef Watch  
**Record:** 1985–2024 | **Baseline:** 1985–2005  
**Location:** Mafia Island MPA, Tanzania (−7.85°N, 39.82°E)

---

## Figures Produced

| # | Title | Script | Status |
|---|-------|--------|--------|
| Fig 1 | Long-Term SST Trend | `03_fig1_trend.py` | ⬜ |
| Fig 2 | Seasonal Climatology Shift | `04_fig2_climatology.py` | ⬜ |
| Fig 3 | Marine Heatwave Analysis | `05_fig3_MHW.py` | ⬜ |
| Fig 4 | Degree Heating Weeks | `06_fig4_DHW.py` | ⬜ |
| Fig 5 | SST Anomaly Heatmap | `07_fig5_heatmap.py` | ⬜ |

---

## Workflow — Step by Step

```
raw data → processed → analysis outputs → figures → factsheet
```

### Step 1 — Download raw data
`scripts/01_download_OISST.py`
- Downloads NOAA OISST v2.1 daily NetCDF files for the MIMP bounding box
- Output: `data/raw/OISST_MIMP_1985_2024.nc`

### Step 2 — Download DHW data
`scripts/02_download_DHW.py`
- Downloads NOAA CRW daily DHW for MIMP
- Output: `data/raw/CRW_DHW_MIMP_1985_2024.nc`

### Step 3 — Process and clean
`scripts/03_process_SST.py`
- Extracts nearest grid cell to MIMP centre point
- Computes daily, monthly, and annual means
- Computes climatological mean and P90 threshold (Hobday et al. 2016)
- Computes SST anomalies against 1985–2005 baseline
- Output: `data/processed/SST_daily.csv`, `SST_monthly.csv`, `SST_climatology.csv`

### Step 4 — Marine Heatwave detection
`scripts/04_detect_MHW.py`
- Applies Hobday et al. (2016) definition: SST > P90 for ≥5 consecutive days
- Classifies intensity category (Moderate / Strong / Severe / Extreme)
- Output: `data/processed/MHW_events.csv`, `MHW_daily_flags.csv`

### Step 5 — Generate figures (one script per figure)
Scripts `05_fig1_trend.py` through `09_fig5_heatmap.py`
- Each script reads from `data/processed/` and writes to `figures/`
- All figures follow style guide in `00_project_docs/style_guide.md`

### Step 6 — Export headline numbers
`scripts/10_export_key_numbers.py`
- Extracts the single most important number from each figure
- Output: `data/outputs/SST_key_numbers.json`

---

## Key Numbers to Report (target outputs)

| Metric | Where used | Notes |
|--------|-----------|-------|
| Warming rate (°C/decade) | Fig 1, Fact sheet headline | WIO typical = +0.2°C/decade |
| Coolest month temperature shift (°C) | Fig 2 | Jun–Sep recovery window |
| Total MHW days (most recent decade vs first) | Fig 3 | Frequency trend |
| Years per decade above DHW≥4 | Fig 4 | Bleaching frequency |
| Years per decade above DHW≥8 | Fig 4 | Mortality frequency |

---

## Directory Contents

```
01_SST/
├── README.md                     ← This file
├── data/
│   ├── raw/                      ← NetCDF downloads (read-only)
│   ├── processed/                ← CSV analysis files
│   └── outputs/                  ← Headline numbers for fact sheet
├── scripts/
│   ├── 01_download_OISST.py
│   ├── 02_download_DHW.py
│   ├── 03_process_SST.py
│   ├── 04_detect_MHW.py
│   ├── 05_fig1_trend.py
│   ├── 06_fig2_climatology.py
│   ├── 07_fig3_MHW.py
│   ├── 08_fig4_DHW.py
│   ├── 09_fig5_heatmap.py
│   └── 10_export_key_numbers.py
├── figures/                      ← Final exported PNGs and SVGs
└── factsheet/                    ← Fact sheet drafts and final files
```

---

## Technical Definitions

### Climatological Mean (clim_mean)
Average SST for a given calendar day using a ±15-day window across baseline years (1985–2005). Approximately 651 observations per calendar day.

### 90th Percentile Threshold (P90)
90th percentile of the same ~651 observations. Per Hobday et al. (2016).

### MHW Intensity Gap
`gap = P90 − clim_mean`  
`intensity = SST − P90` (during confirmed MHW events only)

### MHW Categories
| Category | Threshold |
|----------|-----------|
| Moderate | 1× gap above P90 |
| Strong | 2× gap above P90 |
| Severe | 3× gap above P90 |
| Extreme | ≥3× gap above P90 |

### Degree Heating Weeks (DHW)
Accumulation of SST anomalies above the Maximum Monthly Mean (MMM) over a rolling 12-week window. Units: °C-weeks.  
- DHW ≥ 4 → Bleaching expected  
- DHW ≥ 8 → Significant coral mortality expected
