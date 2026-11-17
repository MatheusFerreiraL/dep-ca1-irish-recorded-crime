# Recorded Crime in Ireland by Garda Station (2003–2025)

Data Exploration & Preparation (DEP), CA1, CCT College Dublin.
Student: Matheus Leandro (2024657). Lecturer: Dr. Muhammad Iqbal.

## Aim

This project explores, cleans and prepares official recorded crime data for Ireland. It covers missing values, outliers, exploratory data analysis, encoding of categorical variables and Principal Component Analysis (PCA).

## Dataset

- Central Statistics Office (CSO), table **CJA11**: *Recorded crime incidents (New Garda Operating Model)*.
- Source: https://data.cso.ie/table/CJA11
- Raw file: `data/raw/CJA11_recorded_crime_by_garda_station.csv` (downloaded 17/11/2026)
- CSO table notes: `data/raw/CJA11_cso_notes.txt`

## How to run

1. Install R (4.x) and the packages: `install.packages(c("dplyr", "tidyr", "ggplot2"))`
2. Open `code/dep_ca1.R` and set the working directory to this folder (the folder with this README).
3. Run the script from top to bottom. Figures are saved in `figures/` and the clean data in `data/clean/`.

## Folder structure

```
code/          R script
data/raw/      original CSO file (not edited)
data/clean/    cleaned data written by the script
figures/       plots used in the poster
```

## Links

- Video presentation: xxxx
