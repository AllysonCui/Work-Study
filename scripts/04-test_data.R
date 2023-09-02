#### Preamble ####
# Purpose: Test on data cleaning.
# Author: Allyson Cui
# Email: allyson.cui@mail.utoronto.ca
# Date: 1 September 2023
# Prerequisites: None

#### Workspace setup ####
library(dplyr)
library(readr)
library(lubridate)

#### Clean data ####

# Read in raw data
raw_data <- read_csv("inputs/data/raw_data.csv")

# Test: Column Classes (Initial)
stopifnot(is.character(raw_data$Category))
stopifnot(is.character(raw_data$`Licence.Address.Line.3`))

# Convert the date columns to Date type
raw_data$Issued <- as.Date(raw_data$Issued, format = "%Y-%m-%d")
raw_data$`Cancel.Date` <- as.Date(raw_data$`Cancel.Date`, format = "%Y-%m-%d")

# Test: Column Classes (Post-conversion)
stopifnot(is.Date(raw_data$Issued))
stopifnot(is.Date(raw_data$`Cancel.Date`))

# Check and handle NA's if necessary
raw_data$Issued[is.na(raw_data$Issued)] <- as.Date('1900-01-01')
raw_data$`Cancel.Date`[is.na(raw_data$`Cancel.Date`)] <- as.Date('3000-01-01')

# Test: Handling Missing Data
stopifnot(all(!is.na(raw_data$Issued)))
stopifnot(all(!is.na(raw_data$`Cancel.Date`)))

# Test: Boundary Conditions
stopifnot(all(raw_data$Issued >= as.Date('1900-01-01')))
stopifnot(all(raw_data$`Cancel.Date` <= as.Date('3000-01-01')))

# Filter, rename columns, and add number_licensed
cleaned_data <- raw_data %>%
  filter(
    Issued <= as.Date("2022-12-31") | 
      `Cancel.Date` >= as.Date("2022-01-01")
  ) %>%
  select(`Licence.Address.Line.3`, Category) %>%
  rename(
    postal_code = `Licence.Address.Line.3`,
    category = Category
  ) %>%
  group_by(postal_code, category) %>%
  summarize(number_licensed = n(), .groups = 'drop')

# Test: Number of Observations and Variables
stopifnot(nrow(cleaned_data) <= nrow(raw_data))
stopifnot(identical(sort(names(cleaned_data)), sort(c("postal_code", "category", "number_licensed"))))

# Test: Duplicates
stopifnot(nrow(cleaned_data) == nrow(distinct(cleaned_data)))