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
# that is easier to use as a column name
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
  "Offences against government, justice procedures and organisation of crime (15)" = "gov_justice",
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
                  "public_order", "road_traffic", "gov_justice", "other")

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
