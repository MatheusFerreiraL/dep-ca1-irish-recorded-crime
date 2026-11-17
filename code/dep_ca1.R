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
# install.packages(c("dplyr", "tidyr", "ggplot2"))

# dplyr for data manipulation, tidyr to reshape from long to wide,
# ggplot2 for the visualisations
library(dplyr)
library(tidyr)
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
