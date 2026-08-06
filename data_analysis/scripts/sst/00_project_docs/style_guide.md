# Style Guide — MAFIA Factsheet Figures

## Colour Palette

### SST / General
| Use | Hex | Name |
|-----|-----|------|
| Primary line (SST) | `#1565C0` | Deep Blue |
| Early period / cool | `#1976D2` | Blue |
| Recent period / warm | `#D32F2F` | Red |
| Trend line | `#212121` | Near-black |
| WIO comparison | `#757575` | Mid-grey |
| Anomaly zero line | `#9E9E9E` | Light-grey |

### MHW Category Colours (Hobday et al. 2018)
| Category | Hex | Name |
|----------|-----|------|
| Moderate | `#FFC107` | Amber |
| Strong | `#FF7043` | Deep Orange |
| Severe | `#D32F2F` | Red |
| Extreme | `#6A1B9A` | Purple |

### DHW Bleaching Alert Zones
| Zone | DHW Range | Hex |
|------|-----------|-----|
| No stress | 0–1 | `#ECEFF1` |
| Watch | 1–4 | `#FFF9C4` |
| Warning | 4–8 | `#FF8F00` |
| Alert Level 1 | 8–12 | `#E53935` |
| Alert Level 2 | ≥12 | `#6A1B9A` |

---

## Figure Standards

| Property | Value |
|----------|-------|
| Figure size (full width) | 12 × 6 inches |
| Figure size (half width) | 6 × 5 inches |
| DPI for export | 300 |
| Font (title) | Arial Bold, 13pt |
| Font (axis labels) | Arial, 11pt |
| Font (tick labels) | Arial, 9pt |
| Font (annotations) | Arial Italic, 9pt |
| Panel labels | Bold, 12pt, upper-left corner `(A)`, `(B)`... |

---

## Annotations

- Bleaching years to annotate: **1998, 2010, 2016, 2024**
- Use a vertical dashed line (`--`, grey `#9E9E9E`) with a small label above
- DHW reference lines: horizontal dashed at 4 and 8 with inline labels
- Baseline period shading: light grey fill `#F5F5F5`, alpha 0.3

---

## File Naming Convention

```
fig{N}_{descriptor}_{YYYYMMDD}.png
```
Examples:
- `fig1_SST_longterm_trend_20250101.png`
- `fig3_MHW_analysis_20250101.png`

Always export both `.png` (for documents) and `.svg` (for editing) where possible.

---

## Caption Style

Captions follow the format:
> **Figure N.** [One sentence describing what the figure shows.] [Data period and source in parentheses.] [Note if synthetic/mock data.]

Example:
> **Figure 1.** Monthly SST at Mafia Island MPA (1985–2024) with linear warming trend and annotated bleaching events. (NOAA OISST v2.1; baseline 1985–2005.)
