#### Preamble ####
# Purpose: Test on data cleaning.
# Author: Allyson Cui
# Email: allyson.cui@mail.utoronto.ca
# Date: 1 September 2023
# Prerequisites: None

#### Preamble ####
# Install and load the testthat package
# install.packages("testthat")
library(testthat)
library(dplyr)
library(readr)
library(lubridate)
library(tidyverse)
library(assertthat)

#### Clean data ####
# Read in raw data
raw_data <- read_csv("inputs/data/raw_data.csv")

# Add assertions to test if raw_data is correctly read
assert_that(nrow(raw_data) > 0)
assert_that(ncol(raw_data) > 0)

cleaned_data <- read_csv("outputs/data/cleaned_data.csv")

# Assertions for cleaned_data
assert_that(all(nchar(cleaned_data$postal_code) >= 6))
assert_that(nrow(cleaned_data) == nrow(distinct(cleaned_data)))
assert_that(!any(is.na(cleaned_data)))
assert_that(all(sapply(cleaned_data[, -1], is.numeric)))

#### Data analysis ####
data_for_correlation <- cleaned_data %>% select(-postal_code)

# Calculate the correlation matrix
cor_matrix <- cor(data_for_correlation, use = "pairwise.complete.obs")

# Assertions for correlation matrix
assert_that(nrow(cor_matrix) == ncol(cor_matrix))
assert_that(all(diag(cor_matrix) == 1))
assert_that(all(cor_matrix >= -1 & cor_matrix <= 1))

# After reading your analysis_data into an R object
analysis_data <- read_csv("outputs/data/analysis_data.csv")

# Assertions for final analysis_data

# Check that the data frame is not empty
assert_that(nrow(analysis_data) > 0)

# Check that the number of columns is as expected (assuming you expect 3 columns: 'category_a', 'category_b', 'correlation_coefficient')
assert_that(ncol(analysis_data) == 3)

# Check that the columns are of the expected types
assert_that(is.character(analysis_data$category_a))
assert_that(is.character(analysis_data$category_b))
assert_that(is.numeric(analysis_data$correlation_coefficient))

# Check that correlation coefficients are within range [-1, 1]
assert_that(all(analysis_data$correlation_coefficient >= -1 & analysis_data$correlation_coefficient <= 1))

# Check for missing values
assert_that(sum(is.na(analysis_data)) == 0)

# Check for duplicates
assert_that(nrow(analysis_data) == nrow(unique(analysis_data)))
