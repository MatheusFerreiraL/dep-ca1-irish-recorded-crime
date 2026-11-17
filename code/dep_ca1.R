# ==========================================================
# DEP CA1 - RECORDED CRIME IN IRELAND BY GARDA STATION
# Data Exploration and Preparation - CCT College Dublin
# Student: Matheus Leandro (2024657)
# Dataset: CSO table CJA11, https://data.cso.ie/table/CJA11
# ==========================================================

# Run this script with the working directory set to the project folder
# (the folder that contains README.md, code/, data/ and figures/).


# STEP 1: LOAD LIBRARIES

# Run once if the packages are missing
# install.packages(c("dplyr", "reshape2", "ggplot2"))

# dplyr for data manipulation, reshape2 to reshape from long to wide,
# ggplot2 for the visualisations
library(dplyr)
library(reshape2)
library(ggplot2)


# STEP 2: LOAD THE RAW CSO DATASET

# The CSO file starts with a byte order mark (BOM), so the encoding is
# set to "UTF-8-BOM" to read the first column name correctly
crime_raw <- read.csv("data/raw/CJA11_recorded_crime_by_garda_station.csv",
                      fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)

# First look at the data: size, variable types and summary
dim(crime_raw)
str(crime_raw)
head(crime_raw)
summary(crime_raw)


# STEP 3: CHECK CONSTANT COLUMNS, DUPLICATES AND MISSING VALUES

# Number of different values in each column. Columns with only one value
# (statistic code, label, unit) give no information and can be removed
sapply(crime_raw, function(x) length(unique(x)))

# Exact duplicate rows
sum(duplicated(crime_raw))

# Missing values in each column
colSums(is.na(crime_raw))

# Where are the missing values? Count them by year and by type of offence
crime_na <- crime_raw[is.na(crime_raw$VALUE), ]
table(crime_na$Year)
table(crime_na$Type.of.Offence)


# STEP 4: REMOVE COLUMNS THAT GIVE NO INFORMATION AND RENAME THE REST

# STATISTIC, Statistic.Label and UNIT have one value only, TLIST.A1. is a
# copy of Year, and C02480V03003 is the offence code that is already inside
# the offence name
crime <- crime_raw %>%
  select(Year, station_code = C03037V05454, station_label = Garda.Station,
         offence = Type.of.Offence, incidents = VALUE)

str(crime)


# STEP 5: CLEAN THE STATION LABELS AND CREATE STATION AND DIVISION COLUMNS

# The labels look like "Bailieboro, Louth/Cavan/Monaghan Division (11101)".
# Some labels are not consistent: "Birr,  Laois/Offaly" has a double space
# and some use "Co." and others "Co". These are fixed first
crime$station_label <- gsub("  ", " ", crime$station_label)
crime$station_label <- gsub("Co\\. ", "Co ", crime$station_label)

# One label does not have the word "Division":
# "Newport, Co Mayo/Roscommon/Longford (22605)". It is corrected by hand
crime$station_label <- gsub("Co Mayo/Roscommon/Longford \\(",
                            "Co Mayo, Mayo/Roscommon/Longford Division (",
                            crime$station_label)

# Division is the text between the last comma and the word "Division"
crime$division <- sub(".*, (.*) Division \\(.*", "\\1", crime$station_label)

# Station name is the label without the division and the code
crime$station <- sub(", [^,]* Division \\(.*", "", crime$station_label)

# Check: there should be 21 divisions under the new Garda Operating Model
table(crime$division)
length(unique(crime$division))


# STEP 6: SHORT NAMES FOR THE OFFENCE TYPES

# The CSO names are very long, so each offence group gets a short name
# that is easier to use as a column name.
# Group 15 is called "court_orders": in the CSO table CJQ06 about 98% of
# group 15 incidents in 2025 are "Offences while in custody, breach of
# court orders (157)", so this name describes what it really measures
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

crime$offence <- offence_names[crime$offence]

# Every offence name must have a short name (no NA here)
table(crime$offence, useNA = "ifany")


# STEP 7: RESHAPE FROM LONG TO WIDE FORMAT

# One row per station and year, one column per type of offence.
# dcast() from reshape2 does the opposite of melt(), which we used in class:
# the formula says which columns identify a row (left of ~) and which
# column gives the new column names (right of ~)
crime_wide <- dcast(crime, Year + station_code + division + station ~ offence,
                    value.var = "incidents")

# Expected: 564 stations x 23 years = 12,972 rows
dim(crime_wide)
head(crime_wide)


# STEP 8: HANDLE THE MISSING VALUES

# All missing values are in fraud for 2023, 2024 and 2025
colSums(is.na(crime_wide))
table(crime_wide$Year[is.na(crime_wide$fraud)])

# The CSO notes explain why: from Q3 2023 the fraud figures only include
# incidents reported by the public, because of a backlog of reports from
# financial institutions. Figures before and after are not comparable.
# The values are missing for a known reason, so they are not missing at
# random. Options:
#  - delete the 1,692 rows: we would lose 3 years of data for the other
#    13 offence types, which are complete
#  - replace with the mean or median: this would create fraud numbers
#    that the CSO says do not exist in a comparable form
#  - keep the NA and use fraud only for 2003-2022 (chosen)
# A flag variable records which rows have no comparable fraud value
crime_wide$fraud_missing <- ifelse(is.na(crime_wide$fraud), 1, 0)
table(crime_wide$fraud_missing)


# STEP 9: CHECK ZERO TOTALS (POSSIBLE HIDDEN MISSING VALUES)

# Total of the 13 offence types that are complete for every year
# (fraud is left out so that all years can be compared)
offence_cols <- c("assault", "dangerous_acts", "kidnapping", "robbery",
                  "burglary", "theft", "drugs", "weapons", "damage",
                  "public_order", "road_traffic", "court_orders", "other")

crime_wide$total_crime <- rowSums(crime_wide[, offence_cols])

# Station-years with very low totals
crime_wide %>% filter(total_crime < 5) %>% arrange(total_crime)

# Laytown drops from 906 incidents in 2023 to 61 in 2024 and 0 in 2025.
# The CSO notes say that from 28 January 2024 policing in Laytown moved
# to the Drogheda sub-district. So these are not real low crime years:
# the crime is now recorded in Drogheda. Laytown 2024 and 2025 are removed
crime_wide %>% filter(station == "Laytown") %>% select(Year, total_crime)

crime_wide <- crime_wide %>%
  filter(!(station == "Laytown" & Year >= 2024))

# Templederry 2020 has 0 incidents. It is a very small rural station with
# 2 to 19 incidents per year, so a zero is possible and it is kept


# STEP 10: FEATURE ENGINEERING

# Flag variable: 1 if the station is in the Dublin Metropolitan Region
crime_wide$dublin <- ifelse(grepl("^D.M.R.", crime_wide$division), 1, 0)
table(crime_wide$dublin)

# Share of crime that is violent (assault, robbery, kidnapping), in %.
# It shows the type of crime, not only the amount. It is NA when the
# total is 0 (Templederry 2020)
crime_wide$violent_share <- ifelse(crime_wide$total_crime > 0,
  100 * (crime_wide$assault + crime_wide$robbery + crime_wide$kidnapping) /
    crime_wide$total_crime, NA)

summary(crime_wide$violent_share)


# STEP 11: IDENTIFY OUTLIERS WITH THE IQR METHOD

# A value is an outlier if it is more than 1.5 x IQR above Q3 or below Q1.
# The check is done on total_crime within each year, so that a station is
# compared with other stations in the same year
crime_wide <- crime_wide %>%
  group_by(Year) %>%
  mutate(q1 = quantile(total_crime, 0.25),
         q3 = quantile(total_crime, 0.75),
         outlier = ifelse(total_crime > q3 + 1.5 * (q3 - q1) |
                          total_crime < q1 - 1.5 * (q3 - q1), 1, 0)) %>%
  ungroup() %>%
  select(-q1, -q3)

# Number of outlier station-years, and the share that are in Dublin
table(crime_wide$outlier)
table(crime_wide$outlier, crime_wide$dublin)

# Stations that are outliers most often
crime_wide %>%
  filter(outlier == 1) %>%
  count(station, division, sort = TRUE) %>%
  head(10)

# The outliers are large urban stations (for example Store Street and
# Pearse Street in Dublin city centre). They are real values, not data
# entry errors, so they are kept. Because they are so large, the counts
# are very right-skewed; this is handled later with a log transformation
# before the PCA, not by deleting stations
summary(crime_wide$total_crime)


# STEP 12: SAVE THE CLEAN DATA

# Final size must be at least 3,000 rows and 7 columns
dim(crime_wide)

write.csv(crime_wide, "data/clean/crime_station_year_clean.csv",
          row.names = FALSE)


# ==========================================================
# EXPLORATORY DATA ANALYSIS (EDA)
# Questions:
#  Q1. How did recorded crime change between 2003 and 2025?
#  Q2. Do Dublin and the rest of the country have different crime profiles?
#  Q3. Which types of crime go together?
#  Q4. What happened to crime in 2020, the first year of COVID-19?
# ==========================================================

# Colours used in all plots (checked for colour-blind readers):
# blue for Dublin / decrease, orange for rest of Ireland, red for increase
col_blue <- "#2a78d6"
col_orange <- "#eb6834"
col_red <- "#e34948"
col_grey <- "#f0efec"

# Same simple theme for every plot
theme_set(theme_minimal(base_size = 13))


# STEP 13: VARIATION - DISTRIBUTION OF TOTAL CRIME PER STATION (Q1, Q2)

# Histogram of the total crime of each station in each year.
# Most stations are small, and a few are very large (right-skewed)
fig_hist <- ggplot(crime_wide, aes(x = total_crime)) +
  geom_histogram(bins = 60, fill = col_blue, colour = "white") +
  labs(title = "Total recorded crime per station and year",
       x = "Recorded crime incidents (13 offence types)",
       y = "Number of station-years")
fig_hist
ggsave("figures/fig-01-histogram-total-crime.png", fig_hist,
       width = 8, height = 5, dpi = 300)

# The same data on a log10 scale. The shape is close to symmetric, which
# shows why a log transformation is useful before PCA
fig_hist_log <- ggplot(crime_wide %>% filter(total_crime > 0),
                       aes(x = total_crime)) +
  geom_histogram(bins = 60, fill = col_blue, colour = "white") +
  scale_x_log10() +
  labs(title = "Total recorded crime per station and year (log scale)",
       x = "Recorded crime incidents, log10 scale",
       y = "Number of station-years")
fig_hist_log
ggsave("figures/fig-02-histogram-total-crime-log.png", fig_hist_log,
       width = 8, height = 5, dpi = 300)

# Skewness check without a package: mean much bigger than median
mean(crime_wide$total_crime)
median(crime_wide$total_crime)


# STEP 14: Q1 - NATIONAL TREND 2003-2025

# Sum all stations for each year
crime_year <- crime_wide %>%
  group_by(Year) %>%
  summarise(total_crime = sum(total_crime))

crime_year

# Year with the highest total
crime_year[which.max(crime_year$total_crime), ]

fig_trend <- ggplot(crime_year, aes(x = Year, y = total_crime)) +
  geom_line(colour = col_blue, linewidth = 1) +
  geom_point(colour = col_blue, size = 2) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  labs(title = "Recorded crime in Ireland, 2003-2025",
       subtitle = "13 offence types (fraud not included), all Garda stations",
       x = "Year", y = "Recorded crime incidents")
fig_trend
ggsave("figures/fig-03-national-trend.png", fig_trend,
       width = 8, height = 5, dpi = 300)

# Trend for each type of offence. First the yearly sum of each offence,
# then melt() to long format so that ggplot can make one panel per offence
offence_year <- crime_wide %>%
  group_by(Year) %>%
  summarise(across(all_of(offence_cols), sum))

offence_year_long <- melt(as.data.frame(offence_year), id.vars = "Year",
                          variable.name = "offence", value.name = "incidents")

fig_offence_trend <- ggplot(offence_year_long,
                            aes(x = Year, y = incidents)) +
  geom_line(colour = col_blue, linewidth = 0.8) +
  facet_wrap(~ offence, scales = "free_y", ncol = 4) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Recorded crime by type of offence, 2003-2025",
       subtitle = "Each panel has its own y-axis scale",
       x = "Year", y = "Recorded crime incidents")
fig_offence_trend
ggsave("figures/fig-04-trend-by-offence.png", fig_offence_trend,
       width = 11, height = 7, dpi = 300)

# Change between 2003 and 2025 for each offence type, in %
offence_change <- data.frame(
  offence = offence_cols,
  y2003 = unlist(offence_year[offence_year$Year == 2003, offence_cols]),
  y2025 = unlist(offence_year[offence_year$Year == 2025, offence_cols]))
offence_change$change_pct <- round(100 * (offence_change$y2025 -
                                   offence_change$y2003) / offence_change$y2003, 1)
offence_change[order(offence_change$change_pct), ]


# STEP 15: Q2 - DUBLIN COMPARED WITH THE REST OF IRELAND

crime_wide$area <- ifelse(crime_wide$dublin == 1, "Dublin", "Rest of Ireland")

# Share of each offence type in the total crime of each area, last year
profile <- crime_wide %>%
  filter(Year == 2025) %>%
  group_by(area) %>%
  summarise(across(all_of(offence_cols), sum))

profile_long <- melt(as.data.frame(profile), id.vars = "area",
                     variable.name = "offence", value.name = "incidents")

profile_long <- profile_long %>%
  group_by(area) %>%
  mutate(share = 100 * incidents / sum(incidents)) %>%
  ungroup()

profile_long %>% arrange(offence, area)

fig_profile <- ggplot(profile_long,
                      aes(x = share, y = reorder(offence, share), fill = area)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("Dublin" = col_blue,
                               "Rest of Ireland" = col_orange)) +
  labs(title = "Crime profile: Dublin and the rest of Ireland, 2025",
       subtitle = "Share of each offence type in the area's total recorded crime",
       x = "Share of total recorded crime (%)", y = NULL, fill = NULL) +
  theme(legend.position = "top")
fig_profile
ggsave("figures/fig-05-profile-dublin-rest.png", fig_profile,
       width = 8, height = 6, dpi = 300)

# Violent share per station (covariation: categorical vs numeric).
# Stations with fewer than 50 incidents are left out, because one or two
# incidents change their percentage a lot
fig_violent <- ggplot(crime_wide %>% filter(Year == 2025, total_crime >= 50),
                      aes(x = area, y = violent_share, fill = area)) +
  geom_boxplot(show.legend = FALSE) +
  scale_fill_manual(values = c("Dublin" = col_blue,
                               "Rest of Ireland" = col_orange)) +
  labs(title = "Share of violent crime per station, 2025",
       subtitle = "Assault, robbery and kidnapping; stations with 50+ incidents",
       x = NULL, y = "Violent crime share (%)")
fig_violent
ggsave("figures/fig-06-violent-share-boxplot.png", fig_violent,
       width = 7, height = 5, dpi = 300)

crime_wide %>%
  filter(Year == 2025, total_crime >= 50) %>%
  group_by(area) %>%
  summarise(stations = n(), median_violent = median(violent_share),
            median_total = median(total_crime))


# STEP 16: UNUSUAL PATTERNS - TOTAL CRIME BY DIVISION

# Boxplot on a log scale: the points above each box are the outliers
fig_division <- ggplot(crime_wide %>% filter(Year == 2025),
                       aes(x = total_crime,
                           y = reorder(division, total_crime, FUN = median),
                           fill = area)) +
  geom_boxplot(outlier.size = 1.5) +
  scale_x_log10(labels = scales::comma) +
  scale_fill_manual(values = c("Dublin" = col_blue,
                               "Rest of Ireland" = col_orange)) +
  labs(title = "Total crime per station by Garda division, 2025",
       x = "Recorded crime incidents per station, log10 scale",
       y = NULL, fill = NULL) +
  theme(legend.position = "top")
fig_division
ggsave("figures/fig-07-division-boxplot.png", fig_division,
       width = 8, height = 7, dpi = 300)

# The ten stations with the most crime in 2025
crime_wide %>%
  filter(Year == 2025) %>%
  arrange(desc(total_crime)) %>%
  select(station, division, total_crime, theft, assault, public_order) %>%
  head(10)


# STEP 17: Q3 - WHICH TYPES OF CRIME GO TOGETHER (CORRELATION)

# Correlation between offence types across stations in 2025.
# log(x + 1) is used because of the outliers and the zeros
crime_2025 <- crime_wide %>% filter(Year == 2025)
cor_matrix <- cor(log(crime_2025[, offence_cols] + 1))
round(cor_matrix, 2)

# Lowest correlations (the pairs that go together least)
cor_long <- melt(cor_matrix, varnames = c("offence_1", "offence_2"),
                 value.name = "correlation")
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
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
fig_cor
ggsave("figures/fig-08-correlation-heatmap.png", fig_cor,
       width = 9, height = 8, dpi = 300)

# Covariation of two offence types: theft and burglary per station
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
  theme(legend.position = "top")
fig_scatter
ggsave("figures/fig-09-scatter-theft-burglary.png", fig_scatter,
       width = 7, height = 6, dpi = 300)


# STEP 18: Q4 - COVID-19: CHANGE FROM 2019 TO 2020

covid <- data.frame(
  offence = offence_cols,
  y2019 = unlist(offence_year[offence_year$Year == 2019, offence_cols]),
  y2020 = unlist(offence_year[offence_year$Year == 2020, offence_cols]))
covid$change_pct <- 100 * (covid$y2020 - covid$y2019) / covid$y2019
covid$direction <- ifelse(covid$change_pct >= 0, "Increase", "Decrease")
covid[order(covid$change_pct), ]

fig_covid <- ggplot(covid, aes(x = change_pct,
                               y = reorder(offence, change_pct),
                               fill = direction)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 0, colour = "grey40") +
  scale_fill_manual(values = c("Decrease" = col_blue, "Increase" = col_red)) +
  labs(title = "Change in recorded crime from 2019 to 2020 (COVID-19)",
       x = "Change (%)", y = NULL, fill = NULL) +
  theme(legend.position = "top")
fig_covid
ggsave("figures/fig-10-covid-change.png", fig_covid,
       width = 8, height = 6, dpi = 300)
