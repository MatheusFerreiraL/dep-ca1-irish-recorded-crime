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

1. Install R (4.6.2 was used) and the packages (dplyr 1.2.1, reshape2 1.4.5, ggplot2 4.0.3, fastDummies 1.7.6): `install.packages(c("dplyr", "reshape2", "ggplot2", "fastDummies"))`
2. Open `code/dep_ca1.R` and set the working directory to this folder (the folder with this README).
3. Run the script from top to bottom. Figures are saved in `figures/` and the clean data in `data/clean/`.

## Folder structure

```
code/          R script
data/raw/      original CSO file (not edited)
data/clean/    cleaned data written by the script
figures/       plots used in the poster
```

## References

- CSO (2026a) *CJA11: Recorded crime incidents (New Garda Operating Model)* [Dataset]. Central Statistics Office. Available at: https://data.cso.ie/table/CJA11 (Accessed: 17 November 2026).
- CSO (2026b) *CJQ06: Recorded crime incidents* [Dataset]. Central Statistics Office. Available at: https://data.cso.ie/table/CJQ06 (Accessed: 17 November 2026).
- Kaplan, J. (2026) *fastDummies: Fast creation of dummy (binary) columns and rows from categorical variables* (Version 1.7.6) [Software]. Available at: https://CRAN.R-project.org/package=fastDummies (Accessed: 20 November 2026).
- R Core Team (2026) *R: A language and environment for statistical computing* (Version 4.6.2) [Software]. Vienna: R Foundation for Statistical Computing. Available at: https://www.R-project.org/ (Accessed: 18 November 2026).
- Wickham, H. (2025) *reshape2: Flexibly reshape data: a reboot of the reshape package* (Version 1.4.5) [Software]. Available at: https://CRAN.R-project.org/package=reshape2 (Accessed: 20 November 2026).
- Wickham, H. et al. (2026a) *dplyr: A grammar of data manipulation* (Version 1.2.1) [Software]. Available at: https://CRAN.R-project.org/package=dplyr (Accessed: 20 November 2026).
- Wickham, H. et al. (2026b) *ggplot2: Create elegant data visualisations using the grammar of graphics* (Version 4.0.3) [Software]. Available at: https://CRAN.R-project.org/package=ggplot2 (Accessed: 20 November 2026).

## Links

- Video presentation: xxxx
