# MAFIA ISLAND MARINE PROTECTED AREA — CLIMATE FACT SHEET REPOSITORY

**Project:** Climate Indicator Fact Sheet Development  
**Area:** Mafia Island MPA (MIMP), Tanzania  
**Ocean region:** Western Indian Ocean (WIO)  
**Data anchor:** NOAA OISST v2.1 | NOAA Coral Reef Watch  
**Baseline period:** 1985–2005  
**Record period:** 1985–2024

---

## Purpose

This repository holds everything needed to reproduce each climate indicator fact sheet for the Mafia Island MPA — from raw data download through processed outputs to the final published figures. Each indicator lives in its own self-contained folder under `indicators/`.

---

## Repository Structure

```
MAFIA_Factsheets/
│
├── README.md                          ← This file
├── 00_project_docs/                   ← Shared documentation
│   ├── PROJECT_NOTES.md               ← Overall project context & decisions
│   ├── data_sources.md                ← All data sources, versions, URLs
│   └── style_guide.md                 ← Colour palette, fonts, figure standards
│
└── indicators/
    ├── 01_SST/                        ← Indicator 1: Sea Surface Temperature
    │   ├── data/
    │   │   ├── raw/                   ← Downloaded source files (never edited)
    │   │   ├── processed/             ← Cleaned, clipped, analysis-ready files
    │   │   └── outputs/               ← Final numbers/tables referenced in factsheet
    │   ├── scripts/                   ← All analysis and plotting scripts
    │   ├── figures/                   ← Final exported figure files (PNG/SVG/PDF)
    │   ├── factsheet/                 ← Fact sheet draft files
    │   └── README.md                  ← SST-specific notes and workflow
    │
    ├── 02_[Next_Indicator]/           ← Placeholder — to be created
    └── ...
```

---

## Indicators Planned

| # | Indicator | Status | Key Data Source |
|---|-----------|--------|----------------|
| 01 | Sea Surface Temperature (SST) | 🟡 In progress | NOAA OISST v2.1 |
| 02 | TBD | ⬜ Pending | — |
| 03 | TBD | ⬜ Pending | — |

---

## How to Use This Repository

1. Start in an indicator's `README.md` for its specific workflow.
2. Run scripts in order — numbered prefixes indicate sequence (e.g. `01_download.py`, `02_process.py`).
3. Raw data files in `data/raw/` are **read-only** — never edit them in place.
4. Final figures ready for the fact sheet go into `figures/`.
5. All key findings (headline numbers) go into `data/outputs/` as `.csv` or `.json`.

---

## Key References

- Hobday et al. (2016). Marine heatwave definition. *Progress in Oceanography*, 141, 227–238.
- Hobday et al. (2018). Categorizing marine heatwaves. *Oceanography*, 31(2), 162–173.
- NOAA CRW (2024). Daily Global 5-km Satellite Coral Bleaching DHW.
- Reynolds et al. (2007). NOAA OISST. *Journal of Climate*, 20(22), 5473–5496.
