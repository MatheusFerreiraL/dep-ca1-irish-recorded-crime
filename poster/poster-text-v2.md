# Recorded Crime in Ireland by Garda Station, 2003–2025: Data Preparation, EDA and PCA

Matheus Leandro (2024657) · BSc (Hons) in Computing in IT · CCT College Dublin · Data Exploration & Preparation, CA1 · November 2026

_Poster text, draft v2 (20/11/2026), after humanizer style pass. Body word count: 687 (excludes headings, figures/captions, code, references and citations; brief limit 700)._

## INTRODUCTION

National crime totals hide big differences between places and types of crime. This project explores recorded crime per Garda station (Population and Society domain): where it is recorded, what kind it is and how it changed.

### Research questions

- Q1. How did recorded crime change from 2003 to 2025?
- Q2. Is crime in Dublin different from the rest of Ireland?
- Q3. Which types of crime go together?
- Q4. What changed in 2020 (COVID-19)?

## DATASET

CSO table CJA11 gives recorded crime incidents by Garda station, year and offence type (CSO, 2026a). It is official open data (CC BY 4.0) with counts only, so no personal data. Raw file: 181,608 rows × 10 columns (564 stations × 23 years × 14 offence groups).

## DATA PREPARATION

Three constant columns were removed in R (R Core Team, 2026). Station labels had errors (double spaces, "Co." vs "Co", one without "Division") and were fixed before splitting them into station and division (21). The data was reshaped to one row per station and year.

```r
crime_wide <- dcast(crime,
  Year + station_code + division + station ~ offence,
  value.var = "incidents")
crime_wide$fraud_missing <-
  ifelse(is.na(crime_wide$fraud), 1, 0)
```

### Missing values

All 1,692 missing values are fraud in 2023–2025, which the CSO says is not comparable with earlier years (CSO, 2026a). Mean imputation would invent numbers, and deleting the rows would also remove 2023–2025 data for the other 13 offences. The NAs were therefore kept with a flag, and fraud was left out of totals and PCA.

### Hidden zeros and new features

Laytown drops to 61 and then 0 incidents because its policing moved to Drogheda in 2024 (CSO, 2026a), so those rows were removed. New features: total_crime, a Dublin flag and violent crime share. Final data: 12,970 rows × 23 columns.

### Outliers

The 1.5 × IQR rule within each year flags 1,807 station-years, 806 in Dublin (Figure 1). These are real busy stations like Store Street (12,355 incidents in 2025), so they were kept. The skew (mean 610, median 145) was handled with a log transformation.

```r
crime_wide <- crime_wide %>%
  group_by(Year) %>%
  mutate(q1 = quantile(total_crime, 0.25),
         q3 = quantile(total_crime, 0.75),
         outlier = ifelse(total_crime >
           q3 + 1.5 * (q3 - q1), 1, 0))
```

*[fig-07-division-boxplot.png] Figure 1: Crime per station by division, 2025*

## EXPLORATORY ANALYSIS

### Q1 – Change over time

Crime rose from 280,277 in 2003 to 505,727 in 2008, then fell to 265,410 in 2021 (Figure 2). Since 2003 burglary fell by 67%, while assault rose by 87%, drugs by 74% and court order breaches by 137%. A sharp jump in road offences in 2006–2008 cannot be explained by the data alone.

*[fig-03-national-trend.png] Figure 2: National trend, 2003–2025*

### Q2 – Dublin and the rest of Ireland

In 2025, 41 Dublin stations (of 563) recorded 41.8% of all crime. Theft is 32.5% of Dublin crime against 22.0% elsewhere, while dangerous acts such as drink driving (CSO, 2026b) are 5.4% against 16.8% (Figure 3).

*[fig-05-profile-dublin-rest.png] Figure 3: Crime profile, Dublin vs rest*

### Q3 – Crimes that go together

All 78 offence pairs are positively correlated (0.41–0.92, Figure 4), mainly because busy stations record more of everything. Kidnapping, a rare offence, has the weakest links.

*[fig-08-correlation-heatmap.png] Figure 4: Correlation between offence types, 2025*

### Q4 – COVID-19

From 2019 to 2020 burglary fell by 35% and theft by 24%, but weapons (+15%), dangerous acts (+11%) and drugs (+8.5%) rose. This fits the lockdowns: fewer people out and more people at home meant fewer chances for theft and burglary.

## ENCODING

Division is nominal (21 values, no order), so label encoding (1–21) would create a false order. One-hot encoding gives 21 dummy columns with one 1 per row.

```r
division_onehot <- dummy_cols(crime_wide["division"],
  select_columns = "division",
  remove_selected_columns = TRUE)
```

## PCA

PCA builds new uncorrelated variables that keep as much variance as possible (Jolliffe and Cadima, 2016). The 13 offence counts were log-transformed and standardised so theft does not dominate. PC1 explains 74.6%; the Kaiser rule (Kaiser, 1960) keeps one component and the 80% rule two (Figure 5). PC1 correlates 0.97 with station size, so it is a "volume of crime" score. PC2 and PC3 each reflect one rare offence.

```r
pca_scaled <- scale(log(crime_wide[, offence_cols] + 1))
pca <- prcomp(pca_scaled, center = FALSE, scale. = FALSE)
```

*[fig-11-pca-scree.png] Figure 5: Scree plot (offence counts)*

A second PCA on offence shares removes size. Its PC1 (19.1%) separates road offences from theft, damage and violence (Figure 6). Dublin sits on the theft side, but so do small rural stations with few road offences, so it is not an urban–rural axis. With the 21 dummies added, 17 components are needed for 80% instead of 2. For comparison, feature selection with a correlation filter (r > 0.9) keeps the original variables and would only drop damage, while PCA replaces them with new components.

*[fig-15-mix-pca-scores.png] Figure 6: Stations by crime mix, 2025*

## CONCLUSIONS

Crime peaked in 2008, and since then the mix moved from burglary and damage towards assault, drugs and court order breaches. Dublin concentrates crime and is theft-heavy. Station size drives most of the variation. Without it, stations differ mainly between road offences on one side and theft, damage and violence on the other. Limitations: recorded crime depends on reporting and policing, fraud is unusable after 2022, and counts are not per population.

## REFERENCES

- CSO (2026a) *CJA11: Recorded crime incidents (New Garda Operating Model)* [Dataset]. Central Statistics Office. Available at: https://data.cso.ie/table/CJA11 (Accessed: 17 November 2026).
- CSO (2026b) *CJQ06: Recorded crime incidents* [Dataset]. Central Statistics Office. Available at: https://data.cso.ie/table/CJQ06 (Accessed: 17 November 2026).
- Jolliffe, I.T. and Cadima, J. (2016) 'Principal component analysis: a review and recent developments', *Philosophical Transactions of the Royal Society A*, 374(2065), 20150202. doi: 10.1098/rsta.2015.0202.
- Kaiser, H.F. (1960) 'The application of electronic computers to factor analysis', *Educational and Psychological Measurement*, 20(1), pp. 141–151. doi: 10.1177/001316446002000116.
- R Core Team (2026) *R: A language and environment for statistical computing* (Version 4.6.2) [Software]. Vienna: R Foundation for Statistical Computing. Available at: https://www.R-project.org/ (Accessed: 18 November 2026).
