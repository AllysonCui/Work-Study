#### Preamble ####
# Purpose: Clean on 2022 Toronto Business Licenses and result in postal codes,
# categories, and number of licensed businesses.
# Author: Allyson Cui
# Email: allyson.cui@mail.utoronto.ca
# Date: 1 September 2023
# Prerequisites: None

#### Workspace setup ####
#install.packages("dplyr")

library(dplyr)
library(readr)
library(lubridate)
library(tidyverse)

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
    Issued <= as.Date("2022-12-31") & 
      `Cancel.Date` >= as.Date("2022-01-01") # Only consider the businesses whose license was valid during some days in the year 2022
  ) %>%
  select(`Licence.Address.Line.3`, Category) %>% # select desired columns
  rename(
    postal_code = `Licence.Address.Line.3`, 
    category = Category
  ) %>% 
  group_by(postal_code, category) %>%
  summarize(number_licensed = n(), .groups = 'drop') # count number of occurrences

# Generate all unique combinations of postal_code and category
all_combinations <- expand_grid(
  postal_code = unique(cleaned_data$postal_code),
  category = unique(cleaned_data$category)
)

# Left join this with your original cleaned_data, filling in missing values with zero
complete_cleaned_data <- left_join(all_combinations, cleaned_data, by = c("postal_code", "category")) %>%
  replace_na(list(number_licensed = 0))

# Reshape the data
reshaped_data <- complete_cleaned_data %>% 
  pivot_wider(names_from = category, values_from = number_licensed, values_fill = 0)

# Reorder the columns in ascending order, keeping 'postal_code' first
reshaped_data <- reshaped_data %>% 
  select(postal_code, sort(names(reshaped_data)[names(reshaped_data) != "postal_code"]))

# Remove rows where postal_code is "NA"
reshaped_data <- reshaped_data %>% 
  filter(postal_code != "NA")

# Remove rows where postal_code has fewer than 6 characters
reshaped_data <- reshaped_data %>% 
  filter(nchar(postal_code) >= 6)

# Remove rows with a single non-zero category (excluding the postal_code column)
reshaped_data <- reshaped_data %>% 
  rowwise() %>% 
  filter(sum(c_across(-1) > 0, na.rm = TRUE) > 1)

# Remove columns with a sum of zero
reshaped_data <- reshaped_data %>%
  select(where(~ sum(. > 0, na.rm = TRUE) > 0))

#### Save data ####
write_csv(reshaped_data, "outputs/data/cleaned_data.csv")