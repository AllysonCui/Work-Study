#### Preamble ####
# Purpose: Test on data cleaning.
# Author: Allyson Cui
# Email: allyson.cui@mail.utoronto.ca
# Date: 1 September 2023
# Prerequisites: None

#### Workspace setup ####
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(tidyverse)

#### Data analysis on correlation coefficients for each unique pairwise categories ####
cleaned_data <- read_csv("outputs/data/cleaned_data.csv")

# Extract unique categories from cleaned_data
unique_categories <- unique(cleaned_data$category)

# Generate unique combinations, avoiding factors
combinations <- expand.grid(category_a = unique_categories, category_b = unique_categories) %>% 
  mutate(across(c(category_a, category_b), as.character)) %>%  
  filter(category_a < category_b)  # Adapt this line as necessary

# Run your correlation calculation
correlation_coefficients <- combinations %>% 
  rowwise() %>% 
  mutate(correlation_coefficient = {
    data_a <- filter(cleaned_data, category == category_a)$number_licensed
    data_b <- filter(cleaned_data, category == category_b)$number_licensed
    
    # make sure data_a and data_b are not empty, else return NA
    if(length(data_a) == 0 || length(data_b) == 0) {
      return(NA)
    }
    
    cor(data_a, data_b, use = "complete.obs")
  }) %>% 
  ungroup()

# Convert to a tibble
correlation_coefficients <- as_tibble(correlation_coefficients)

#### Save data ####
write_csv(correlation_coefficients, "outputs/data/analysis_data.csv")