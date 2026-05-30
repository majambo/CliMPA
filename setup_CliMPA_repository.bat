@echo off
:: ============================================================
:: MAFIA Island MPA — CliMPA Factsheet Repository Setup
:: Base folder: E:\Git_Repositories\CliMPA
:: Run this script once to create the full folder structure.
:: ============================================================

set BASE=E:\Git_Repositories\CliMPA

echo Creating CliMPA repository at %BASE%...
echo.

:: ── Top-level ────────────────────────────────────────────────
mkdir "%BASE%"
mkdir "%BASE%\00_project_docs"

:: ── Indicator 01: Sea Surface Temperature ────────────────────
mkdir "%BASE%\indicators\01_SST"
mkdir "%BASE%\indicators\01_SST\data\raw"
mkdir "%BASE%\indicators\01_SST\data\processed"
mkdir "%BASE%\indicators\01_SST\data\outputs"
mkdir "%BASE%\indicators\01_SST\scripts"
mkdir "%BASE%\indicators\01_SST\figures"
mkdir "%BASE%\indicators\01_SST\factsheet"

:: ── Placeholder for future indicators ────────────────────────
:: Uncomment and rename when you start each new indicator
:: mkdir "%BASE%\indicators\02_OA"
:: mkdir "%BASE%\indicators\02_OA\data\raw"
:: mkdir "%BASE%\indicators\02_OA\data\processed"
:: mkdir "%BASE%\indicators\02_OA\data\outputs"
:: mkdir "%BASE%\indicators\02_OA\scripts"
:: mkdir "%BASE%\indicators\02_OA\figures"
:: mkdir "%BASE%\indicators\02_OA\factsheet"

echo.
echo ============================================================
echo  DONE. Folder structure created at:
echo  %BASE%
echo ============================================================
echo.
echo Next steps:
echo  1. Copy the Python scripts into:
echo     %BASE%\indicators\01_SST\scripts\
echo  2. Copy the project docs into:
echo     %BASE%\00_project_docs\
echo  3. Copy README.md files into their respective folders
echo  4. Run: python 01_download_OISST.py  (from the scripts folder)
echo.
pause
