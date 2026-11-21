# DEP CA1 - Recorded crime in Ireland by Garda station (2003-2025)
# Matheus Leandro - 2024657
# Data: CSO table CJA11, https://data.cso.ie/table/CJA11
# Working directory: the project folder (with README.md, code/, data/)

# Packages (full Harvard references in README.md):
# dplyr 1.2.1 (Wickham et al., 2026a), reshape2 1.4.5 (Wickham, 2025),
# ggplot2 4.0.3 (Wickham et al., 2026b), fastDummies 1.7.6 (Kaplan, 2026)
# install.packages(c("dplyr", "reshape2", "ggplot2", "fastDummies"))
library(dplyr)
library(reshape2)
library(ggplot2)
library(fastDummies)


# ---- 1. Load and check the raw data ----

# the CSO file has a BOM at the start, so the encoding is needed
crime_raw <- read.csv("data/raw/CJA11_recorded_crime_by_garda_station.csv",
                      fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)

# size, column types and a first summary of every column
dim(crime_raw)
str(crime_raw)
summary(crime_raw)

# these columns only have one value
table(crime_raw$STATISTIC)
table(crime_raw$Statistic.Label)
table(crime_raw$UNIT)

# no duplicate rows, and the only NAs are in VALUE (1,692)
sum(duplicated(crime_raw))
colSums(is.na(crime_raw))

# which years and offence types the NAs come from
crime_na <- crime_raw[is.na(crime_raw$VALUE), ]
table(crime_na$Year)
table(crime_na$Type.of.Offence)


# ---- 2. Select and rename columns ----

# keep 5 of the 10 columns and give them short names; the others are
# constant or only codes for the year and the offence
crime <- crime_raw %>%
  select(Year, station_code = C03037V05454, station_label = Garda.Station,
         offence = Type.of.Offence, incidents = VALUE)


# ---- 3. Clean the station labels ----

# labels look like "Bailieboro, Louth/Cavan/Monaghan Division (11101)"
# some have a double space or "Co." instead of "Co"
crime$station_label <- gsub("  ", " ", crime$station_label)
crime$station_label <- gsub("Co\\. ", "Co ", crime$station_label)

# Newport (22605) is missing the word "Division", fixed by hand
crime$station_label <- gsub("Co Mayo/Roscommon/Longford \\(",
                            "Co Mayo, Mayo/Roscommon/Longford Division (",
                            crime$station_label)

# division = the text between ", " and " Division ("
# station = everything before ", <division> Division ("
crime$division <- sub(".*, (.*) Division \\(.*", "\\1", crime$station_label)
crime$station <- sub(", [^,]* Division \\(.*", "", crime$station_label)

# should be 21 divisions
table(crime$division)
length(unique(crime$division))


# ---- 4. Short names for the offence types ----

offence_names <- c(
  "Attempts/threats to murder, assaults, harassments and related offences (03)" = "assault",
  "Dangerous or negligent acts (04)" = "dangerous_acts",
  "Kidnapping and related offences (05)" = "kidnapping",
  "Robbery, extortion and hijacking offences (06)" = "robbery",
  "Burglary and related offences (07)" = "burglary",
  "Theft and related offences (08)" = "theft",
  "Fraud, deception and related offences (09)" = "fraud",
  "Controlled drug offences (10)" = "drugs",
  "Weapons and explosives offences (11)" = "weapons",
  "Damage to property and to the environment  offences (12)" = "damage",
  "Public order and other social code offences (13)" = "public_order",
  "Road and traffic offences (14)" = "road_traffic",
  "Offences against government, justice procedures and organisation of crime (15)" = "court_orders",
  "Offences not elsewhere classified (16)" = "other")

# group 15 is called court_orders because about 98% of it is
# "breach of court orders" (CSO table CJQ06)
crime$offence <- offence_names[crime$offence]
# no NA here means every offence name was matched
table(crime$offence, useNA = "ifany")


# ---- 5. Long to wide format ----

# one row per station and year, one column per offence type
# dcast() is the opposite of melt()
crime_wide <- dcast(crime, Year + station_code + division + station ~ offence,
                    value.var = "incidents")

# 564 stations x 23 years = 12,972 rows
dim(crime_wide)
head(crime_wide)


# ---- 6. Missing values ----

colSums(is.na(crime_wide))
table(crime_wide$Year[is.na(crime_wide$fraud)])

# fraud is missing for every station in 2023-2025. The CSO says fraud
# after Q3 2023 is not comparable with earlier years, so the NAs are kept
# (no mean filling) and fraud is only used up to 2022
crime_wide$fraud_missing <- ifelse(is.na(crime_wide$fraud), 1, 0)
table(crime_wide$fraud_missing)


# ---- 7. Zero totals ----

offence_cols <- c("assault", "dangerous_acts", "kidnapping", "robbery",
                  "burglary", "theft", "drugs", "weapons", "damage",
                  "public_order", "road_traffic", "court_orders", "other")

# total without fraud, so all years can be compared
crime_wide$total_crime <- rowSums(crime_wide[, offence_cols])

# stations with almost no crime, to look for closed or merged stations
crime_wide %>% filter(total_crime < 5) %>% arrange(total_crime)

# Laytown drops to 61 in 2024 and 0 in 2025 because its policing moved
# to Drogheda in January 2024, so those two rows are removed
crime_wide %>% filter(station == "Laytown") %>% select(Year, total_crime)

crime_wide <- crime_wide %>%
  filter(!(station == "Laytown" & Year >= 2024))


# ---- 8. New features ----

# 1 = station in one of the six Dublin (D.M.R.) divisions
dublin_divisions <- c("D.M.R. Eastern", "D.M.R. North", "D.M.R. North Central",
                      "D.M.R. South", "D.M.R. South Central", "D.M.R. Western")
crime_wide$dublin <- ifelse(crime_wide$division %in% dublin_divisions, 1, 0)
table(crime_wide$dublin)

# % of crime that is violent; NA when the total is 0
crime_wide$violent_share <- ifelse(crime_wide$total_crime > 0,
  100 * (crime_wide$assault + crime_wide$robbery + crime_wide$kidnapping) /
    crime_wide$total_crime, NA)

summary(crime_wide$violent_share)


# ---- 9. Outliers (IQR rule) ----

# each station is compared with the other stations in the same year
crime_wide <- crime_wide %>%
  group_by(Year) %>%
  mutate(q1 = quantile(total_crime, 0.25),
         q3 = quantile(total_crime, 0.75),
         outlier = ifelse(total_crime > q3 + 1.5 * (q3 - q1) |
                          total_crime < q1 - 1.5 * (q3 - q1), 1, 0)) %>%
  ungroup() %>%
  select(-q1, -q3)

# how many outliers, how many are in Dublin, and the stations most often flagged
table(crime_wide$outlier)
table(crime_wide$outlier, crime_wide$dublin)

crime_wide %>%
  filter(outlier == 1) %>%
  count(station, division, sort = TRUE) %>%
  head(10)

# the outliers are big city stations (Store Street, Pearse Street...),
# real values and not errors, so they stay. The skew is dealt with by
# a log transformation before the PCA
summary(crime_wide$total_crime)


# ---- 10. Save the clean data ----

# 12,970 rows and 23 columns after cleaning (the brief asks for 3000 x 7)
dim(crime_wide)
write.csv(crime_wide, "data/clean/crime_station_year_clean.csv",
          row.names = FALSE)


# ==== EDA ====
# Q1 How did recorded crime change from 2003 to 2025?
# Q2 Is crime in Dublin different from the rest of the country?
# Q3 Which types of crime go together?
# Q4 What happened in 2020 (COVID-19)?

# colours checked to be readable for colour-blind readers
col_blue <- "#2a78d6"
col_orange <- "#eb6834"
col_red <- "#e34948"
col_grey <- "#f0efec"

crime_wide$area <- ifelse(crime_wide$dublin == 1, "Dublin", "Rest of Ireland")

# offence counts in long format, used for several plots
crime_long <- melt(as.data.frame(crime_wide[, c("Year", "area", offence_cols)]),
                   id.vars = c("Year", "area"),
                   variable.name = "offence", value.name = "incidents")


# ---- 11. Distribution of total crime ----

# total crime is very right-skewed, so the same histogram is also
# drawn on a log scale
fig_hist <- ggplot(crime_wide, aes(x = total_crime)) +
  geom_histogram(bins = 60, fill = col_blue, colour = "white") +
  labs(title = "Total recorded crime per station and year",
       x = "Recorded crime incidents (13 offence types)",
       y = "Number of station-years") +
  theme_minimal()
fig_hist
ggsave("figures/fig-01-histogram-total-crime.png", fig_hist,
       width = 8, height = 5, dpi = 300)

fig_hist_log <- ggplot(crime_wide %>% filter(total_crime > 0),
                       aes(x = total_crime)) +
  geom_histogram(bins = 60, fill = col_blue, colour = "white") +
  scale_x_log10() +
  labs(title = "Total recorded crime per station and year (log scale)",
       x = "Recorded crime incidents, log10 scale",
       y = "Number of station-years") +
  theme_minimal()
fig_hist_log
ggsave("figures/fig-02-histogram-total-crime-log.png", fig_hist_log,
       width = 8, height = 5, dpi = 300)

# the mean is much higher than the median, another sign of the skew
mean(crime_wide$total_crime)
median(crime_wide$total_crime)


# ---- 12. Q1 Trend over time ----

# national total per year
crime_year <- crime_wide %>%
  group_by(Year) %>%
  summarise(total_crime = sum(total_crime))

crime_year
# the peak year
crime_year[which.max(crime_year$total_crime), ]

fig_trend <- ggplot(crime_year, aes(x = Year, y = total_crime / 1000)) +
  geom_line(colour = col_blue, linewidth = 1) +
  geom_point(colour = col_blue, size = 2) +
  ylim(0, NA) +
  labs(title = "Recorded crime in Ireland, 2003-2025",
       subtitle = "13 offence types (fraud not included), all Garda stations",
       x = "Year", y = "Recorded crime incidents (thousands)") +
  theme_minimal()
fig_trend
ggsave("figures/fig-03-national-trend.png", fig_trend,
       width = 8, height = 5, dpi = 300)

# national total per offence type and year
offence_year <- crime_long %>%
  group_by(Year, offence) %>%
  summarise(incidents = sum(incidents))

fig_offence_trend <- ggplot(offence_year, aes(x = Year, y = incidents / 1000)) +
  geom_line(colour = col_blue, linewidth = 0.8) +
  facet_wrap(~ offence, scales = "free_y", ncol = 4) +
  labs(title = "Recorded crime by type of offence, 2003-2025",
       subtitle = "Each panel has its own y-axis scale",
       x = "Year", y = "Recorded crime incidents (thousands)") +
  theme_minimal()
fig_offence_trend
ggsave("figures/fig-04-trend-by-offence.png", fig_offence_trend,
       width = 11, height = 7, dpi = 300)

# % change 2003 -> 2025 for each offence type
offence_change <- dcast(offence_year %>% filter(Year %in% c(2003, 2025)),
                        offence ~ Year, value.var = "incidents")
names(offence_change) <- c("offence", "y2003", "y2025")
offence_change$change_pct <- round(100 * (offence_change$y2025 -
                                   offence_change$y2003) / offence_change$y2003, 1)
offence_change %>% arrange(change_pct)


# ---- 13. Q2 Dublin vs rest of Ireland ----

# share of each offence type inside Dublin and inside the rest of Ireland
# in 2025, so two areas of different size can be compared
profile <- crime_long %>%
  filter(Year == 2025) %>%
  group_by(area, offence) %>%
  summarise(incidents = sum(incidents)) %>%
  group_by(area) %>%
  mutate(share = 100 * incidents / sum(incidents)) %>%
  ungroup()

profile %>% arrange(offence, area)

fig_profile <- ggplot(profile,
                      aes(x = share, y = reorder(offence, share), fill = area)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("Dublin" = col_blue,
                               "Rest of Ireland" = col_orange)) +
  labs(title = "Crime profile: Dublin and the rest of Ireland, 2025",
       subtitle = "Share of each offence type in the area's total recorded crime",
       x = "Share of total recorded crime (%)", y = NULL, fill = NULL) +
  theme_minimal() +
  theme(legend.position = "top")
fig_profile
ggsave("figures/fig-05-profile-dublin-rest.png", fig_profile,
       width = 8, height = 6, dpi = 300)

# stations under 50 incidents are left out: one or two cases would
# change their percentage too much
fig_violent <- ggplot(crime_wide %>% filter(Year == 2025, total_crime >= 50),
                      aes(x = area, y = violent_share, fill = area)) +
  geom_boxplot(show.legend = FALSE) +
  scale_fill_manual(values = c("Dublin" = col_blue,
                               "Rest of Ireland" = col_orange)) +
  labs(title = "Share of violent crime per station, 2025",
       subtitle = "Assault, robbery and kidnapping; stations with 50+ incidents",
       x = NULL, y = "Violent crime share (%)") +
  theme_minimal()
fig_violent
ggsave("figures/fig-06-violent-share-boxplot.png", fig_violent,
       width = 7, height = 5, dpi = 300)

# median violent share and median total per station in each area
crime_wide %>%
  filter(Year == 2025, total_crime >= 50) %>%
  group_by(area) %>%
  summarise(stations = n(), median_violent = median(violent_share),
            median_total = median(total_crime))


# ---- 14. Unusual patterns by division ----

# one boxplot per division, ordered by the median, to see which divisions
# and stations stand out
fig_division <- ggplot(crime_wide %>% filter(Year == 2025),
                       aes(x = total_crime,
                           y = reorder(division, total_crime, FUN = median),
                           fill = area)) +
  geom_boxplot(outlier.size = 1.5) +
  scale_x_log10() +
  scale_fill_manual(values = c("Dublin" = col_blue,
                               "Rest of Ireland" = col_orange)) +
  labs(title = "Total crime per station by Garda division, 2025",
       x = "Recorded crime incidents per station, log10 scale",
       y = NULL, fill = NULL) +
  theme_minimal() +
  theme(legend.position = "top")
fig_division
ggsave("figures/fig-07-division-boxplot.png", fig_division,
       width = 8, height = 7, dpi = 300)

# the 10 busiest stations in 2025
crime_wide %>%
  filter(Year == 2025) %>%
  arrange(desc(total_crime)) %>%
  select(station, division, total_crime, theft, assault, public_order) %>%
  head(10)


# ---- 15. Q3 Correlation between offence types ----

# log(x + 1) because of the outliers and the zeros
crime_2025 <- crime_wide %>% filter(Year == 2025)
cor_matrix <- cor(log(crime_2025[, offence_cols] + 1))
round(cor_matrix, 2)

cor_long <- melt(cor_matrix, varnames = c("offence_1", "offence_2"),
                 value.name = "correlation")
# each pair is in the matrix twice (and each offence with itself), so
# only one copy is kept to list the 5 weakest pairs
cor_long %>%
  filter(as.character(offence_1) < as.character(offence_2)) %>%
  arrange(correlation) %>%
  head(5)

fig_cor <- ggplot(cor_long, aes(x = offence_1, y = offence_2,
                                fill = correlation)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = round(correlation, 2)), size = 2.8) +
  scale_fill_gradient2(low = col_blue, mid = col_grey, high = col_red,
                       midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Correlation between offence types per station, 2025",
       subtitle = "Pearson correlation of log(incidents + 1)",
       x = NULL, y = NULL, fill = "r") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
fig_cor
ggsave("figures/fig-08-correlation-heatmap.png", fig_cor,
       width = 9, height = 8, dpi = 300)

# one pair in detail, station by station
fig_scatter <- ggplot(crime_2025, aes(x = burglary + 1, y = theft + 1,
                                      colour = area)) +
  geom_point(size = 2, alpha = 0.7) +
  scale_x_log10() +
  scale_y_log10() +
  scale_colour_manual(values = c("Dublin" = col_blue,
                                 "Rest of Ireland" = col_orange)) +
  labs(title = "Theft and burglary per station, 2025",
       x = "Burglary incidents + 1 (log10)", y = "Theft incidents + 1 (log10)",
       colour = NULL) +
  theme_minimal() +
  theme(legend.position = "top")
fig_scatter
ggsave("figures/fig-09-scatter-theft-burglary.png", fig_scatter,
       width = 7, height = 6, dpi = 300)


# ---- 16. Q4 COVID-19 (2019 -> 2020) ----

# % change of each offence type from 2019 to 2020
covid <- dcast(offence_year %>% filter(Year %in% c(2019, 2020)),
               offence ~ Year, value.var = "incidents")
names(covid) <- c("offence", "y2019", "y2020")
covid$change_pct <- 100 * (covid$y2020 - covid$y2019) / covid$y2019
covid$direction <- ifelse(covid$change_pct >= 0, "Increase", "Decrease")
covid %>% arrange(change_pct)

fig_covid <- ggplot(covid, aes(x = change_pct,
                               y = reorder(offence, change_pct),
                               fill = direction)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 0, colour = "grey40") +
  scale_fill_manual(values = c("Decrease" = col_blue, "Increase" = col_red)) +
  labs(title = "Change in recorded crime from 2019 to 2020 (COVID-19)",
       x = "Change (%)", y = NULL, fill = NULL) +
  theme_minimal() +
  theme(legend.position = "top")
fig_covid
ggsave("figures/fig-10-covid-change.png", fig_covid,
       width = 8, height = 6, dpi = 300)


# ==== ENCODING AND PCA ====

# ---- 17. Encoding the division ----

# 21 divisions, stored as a factor
crime_wide$division <- factor(crime_wide$division)
nlevels(crime_wide$division)

# label encoding gives numbers 1-21, but divisions have no order,
# so it would make e.g. Galway "bigger" than Donegal
division_label <- as.integer(crime_wide$division)
head(data.frame(division = crime_wide$division, label = division_label))

# one-hot encoding instead: one 0/1 column per division
division_onehot <- dummy_cols(crime_wide["division"],
                              select_columns = "division",
                              remove_selected_columns = TRUE)
dim(division_onehot)
head(division_onehot[, 1:4])

# every row should have exactly one 1
table(rowSums(division_onehot))

# encoded data set: offence counts, dublin flag and 21 dummies (36 columns)
crime_encoded <- cbind(crime_wide[, c("Year", offence_cols, "dublin")],
                       division_onehot)
dim(crime_encoded)


# ---- 18. PCA on the offence counts ----

# fraud is left out because of the missing years
pca_data <- crime_wide[, offence_cols]

# log first because the counts are very skewed (+1 because of zeros),
# then z-scores so big offence types like theft don't dominate
pca_log <- log(pca_data + 1)
pca_scaled <- scale(pca_log)

# check: every column now has mean 0 and sd 1
round(colMeans(pca_scaled), 3)
round(apply(pca_scaled, 2, sd), 3)

# prcomp() is the base R function for PCA; the data is already scaled
pca <- prcomp(pca_scaled, center = FALSE, scale. = FALSE)
summary(pca)

# all PC1 loadings came out negative. The sign doesn't matter in PCA, so
# I flip it to make a higher PC1 mean more crime
pca$rotation[, 1] <- -pca$rotation[, 1]
pca$x[, 1] <- -pca$x[, 1]


# ---- 19. How many components to keep ----

# eigenvalue = variance of each component (sdev squared)
eigenvalues <- pca$sdev^2
variance_pct <- 100 * eigenvalues / sum(eigenvalues)

pca_variance <- data.frame(component = 1:length(eigenvalues),
                           eigenvalue = round(eigenvalues, 3),
                           variance_pct = round(variance_pct, 1),
                           cumulative_pct = round(cumsum(variance_pct), 1))
pca_variance

# Kaiser rule (eigenvalue > 1) keeps 1 component,
# 80% of the variance keeps 2
sum(eigenvalues > 1)
which(cumsum(variance_pct) >= 80)[1]

fig_scree <- ggplot(pca_variance, aes(x = component, y = variance_pct)) +
  geom_col(fill = col_blue, width = 0.7) +
  geom_line(aes(y = cumulative_pct), colour = col_orange, linewidth = 1) +
  geom_point(aes(y = cumulative_pct), colour = col_orange, size = 2) +
  geom_hline(yintercept = 80, linetype = "dashed", colour = "grey40") +
  scale_x_continuous(breaks = 1:13) +
  labs(title = "PCA scree plot",
       subtitle = "Bars: variance explained by each component. Line: cumulative variance",
       x = "Principal component", y = "Variance explained (%)") +
  theme_minimal()
fig_scree
ggsave("figures/fig-11-pca-scree.png", fig_scree,
       width = 8, height = 5, dpi = 300)


# ---- 20. Loadings and scores ----

# loadings = weight of each offence type in PC1-PC3
loadings <- pca$rotation[, 1:3]
round(loadings, 2)

loadings_long <- melt(loadings, varnames = c("offence", "component"),
                      value.name = "loading")
loadings_long$sign <- ifelse(loadings_long$loading >= 0, "Positive", "Negative")

fig_loadings <- ggplot(loadings_long,
                       aes(x = loading, y = offence, fill = sign)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 0, colour = "grey40") +
  facet_wrap(~ component, ncol = 3) +
  scale_fill_manual(values = c("Negative" = col_blue, "Positive" = col_red)) +
  labs(title = "PCA loadings of the first three components",
       x = "Loading", y = NULL, fill = NULL) +
  theme_minimal() +
  theme(legend.position = "top")
fig_loadings
ggsave("figures/fig-12-pca-loadings.png", fig_loadings,
       width = 10, height = 6, dpi = 300)

# scores = position of each station-year on PC1-PC3
pca_scores <- data.frame(crime_wide[, c("Year", "station", "division",
                                        "area", "total_crime")],
                         pca$x[, 1:3])

# PC1 is basically the size of the station (r = 0.97)
cor(pca_scores$PC1, log(pca_scores$total_crime + 1))

# average scores for Dublin and the rest of Ireland
pca_scores %>%
  group_by(area) %>%
  summarise(PC1 = mean(PC1), PC2 = mean(PC2), PC3 = mean(PC3))

# PC2 and PC3 are almost only kidnapping and "other", two rare offences,
# so they don't say much. That is why I also run a PCA on the crime mix

fig_scores <- ggplot(pca_scores %>% filter(Year == 2025),
                     aes(x = PC1, y = PC2, colour = area)) +
  geom_point(size = 2, alpha = 0.7) +
  scale_colour_manual(values = c("Dublin" = col_blue,
                                 "Rest of Ireland" = col_orange)) +
  labs(title = "Garda stations on the first two principal components, 2025",
       x = paste0("PC1 (", round(variance_pct[1], 1), "% of variance)"),
       y = paste0("PC2 (", round(variance_pct[2], 1), "% of variance)"),
       colour = NULL) +
  theme_minimal() +
  theme(legend.position = "top")
fig_scores
ggsave("figures/fig-13-pca-scores.png", fig_scores,
       width = 8, height = 6, dpi = 300)


# ---- 21. PCA on the crime mix ----

# share (%) of each offence type, to remove the effect of station size
# stations under 50 incidents are left out, as in section 13
mix_data <- crime_wide %>% filter(total_crime >= 50)
nrow(mix_data)

mix_share <- 100 * mix_data[, offence_cols] / mix_data$total_crime

pca_mix <- prcomp(mix_share, center = TRUE, scale. = TRUE)
summary(pca_mix)

# the shares add up to 100, so the last eigenvalue is 0
eigen_mix <- pca_mix$sdev^2
round(eigen_mix, 2)
variance_mix <- 100 * eigen_mix / sum(eigen_mix)
# 5 components have eigenvalue > 1 and 9 are needed for 80%, so the
# crime mix is much less correlated than the counts
sum(eigen_mix > 1)
which(cumsum(variance_mix) >= 80)[1]

# flipped so a high PC1 = more theft, damage and violence
pca_mix$rotation[, 1] <- -pca_mix$rotation[, 1]
pca_mix$x[, 1] <- -pca_mix$x[, 1]
round(pca_mix$rotation[, 1:2], 2)

mix_scores <- data.frame(mix_data[, c("Year", "station", "division", "area",
                                      "total_crime")], pca_mix$x[, 1:2])

mix_scores %>%
  group_by(area) %>%
  summarise(PC1 = mean(PC1), PC2 = mean(PC2))

# the top of the list are small stations with almost no road offences,
# not big cities, so this is not an urban vs rural axis
mix_scores %>% filter(Year == 2025) %>% arrange(desc(PC1)) %>%
  select(station, division, total_crime, PC1) %>% head(5)
mix_scores %>% filter(Year == 2025) %>% arrange(PC1) %>%
  select(station, division, total_crime, PC1) %>% head(5)

mix_loadings <- data.frame(offence = offence_cols,
                           loading = pca_mix$rotation[, 1])
mix_loadings$sign <- ifelse(mix_loadings$loading >= 0, "Positive", "Negative")

fig_mix_loadings <- ggplot(mix_loadings,
                           aes(x = loading, y = reorder(offence, loading),
                               fill = sign)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 0, colour = "grey40") +
  scale_fill_manual(values = c("Negative" = col_blue, "Positive" = col_red)) +
  labs(title = "Crime mix PCA: loadings of PC1",
       subtitle = paste0("PC1 explains ", round(variance_mix[1], 1),
                         "% of the variance in the crime mix"),
       x = "Loading", y = NULL, fill = NULL) +
  theme_minimal() +
  theme(legend.position = "top")
fig_mix_loadings
ggsave("figures/fig-14-mix-pca-loadings.png", fig_mix_loadings,
       width = 8, height = 6, dpi = 300)

fig_mix_scores <- ggplot(mix_scores %>% filter(Year == 2025),
                         aes(x = PC1, y = PC2, colour = area)) +
  geom_point(size = 2, alpha = 0.7) +
  scale_colour_manual(values = c("Dublin" = col_blue,
                                 "Rest of Ireland" = col_orange)) +
  labs(title = "Garda stations by crime mix, 2025",
       subtitle = "Stations with 50+ incidents",
       x = paste0("Mix PC1 (", round(variance_mix[1], 1),
                  "%): road offences <-> theft, damage, violence"),
       y = paste0("Mix PC2 (", round(variance_mix[2], 1), "%)"),
       colour = NULL) +
  theme_minimal() +
  theme(legend.position = "top")
fig_mix_scores
ggsave("figures/fig-15-mix-pca-scores.png", fig_mix_scores,
       width = 8, height = 6, dpi = 300)


# ---- 22. PCA with the division dummies (comparison) ----

# with 21 extra 0/1 columns (34 in total) the variance spreads out a lot
# more: PC1 falls to 29.8% and 17 components are needed for 80%
pca_with_dummies <- prcomp(cbind(pca_log, division_onehot),
                           center = TRUE, scale. = TRUE)
variance_dummies <- 100 * pca_with_dummies$sdev^2 / sum(pca_with_dummies$sdev^2)
ncol(cbind(pca_log, division_onehot))
which(cumsum(variance_dummies) >= 80)[1]
round(variance_dummies[1:3], 1)


# ---- 23. Feature selection vs PCA ----

# filter method: drop one variable from each pair with r > 0.9
cor_all <- cor(pca_log)
cor_all_long <- melt(cor_all, varnames = c("var_1", "var_2"),
                     value.name = "r")
high_pairs <- cor_all_long %>%
  filter(as.character(var_1) < as.character(var_2), abs(r) > 0.9)
high_pairs

# keep all columns except the first one of each pair. Only damage is
# dropped (r = 0.91 with theft), so 12 of 13 stay. Unlike PCA, the kept
# columns are still the real offence counts
selected <- setdiff(offence_cols, as.character(high_pairs$var_1))
selected
length(selected)

# save the PCA scores (PC1-PC3) for each station-year
write.csv(pca_scores, "data/clean/pca_scores.csv", row.names = FALSE)
