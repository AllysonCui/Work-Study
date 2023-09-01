#### Preamble ####
# Purpose: Cleans the raw plane data recorded by two observers..... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 6 April 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]

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
      `Cancel.Date` >= as.Date("2022-01-01")
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