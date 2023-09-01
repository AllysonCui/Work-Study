#### Preamble ####
# Purpose: Clean on 2022 Toronto Business Licenses and result in postal codes,
# categories, and number of licensed businesses.
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

# Convert the date columns to Date type
raw_data$Issued <- as.Date(raw_data$Issued, format = "%Y-%m-%d")
raw_data$`Cancel.Date` <- as.Date(raw_data$`Cancel.Date`, format = "%Y-%m-%d")

# Check and handle NA's if necessary
raw_data$Issued[is.na(raw_data$Issued)] <- as.Date('1900-01-01')
raw_data$`Cancel.Date`[is.na(raw_data$`Cancel.Date`)] <- as.Date('3000-01-01')

# Filter, rename columns, and add number_licensed
cleaned_data <- raw_data %>%
  filter(
    Issued <= as.Date("2022-12-31") | 
      `Cancel.Date` >= as.Date("2022-01-01") # Only consider the businesses 
    # whose license was valid during some days in the year 2022
  ) %>%
  select(`Licence.Address.Line.3`, Category) %>% # select desired columns
  rename(
    postal_code = `Licence.Address.Line.3`, 
    category = Category
  ) %>% 
  group_by(postal_code, category) %>%
  summarize(number_licensed = n(), .groups = 'drop') # count number of occurrences

#### Save data ####
write_csv(cleaned_data, "outputs/data/cleaned_data.csv")