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
install.packages("psych")
library(psych)

#### Data analysis on correlation coefficients for each unique pairwise categories ####
cleaned_data <- read_csv("outputs/data/cleaned_data.csv")

# Remove 'postal_code'
data_for_correlation <- reshaped_data %>% select(-postal_code)

# Calculate correlation matrix
cor_matrix <- cor(data_for_correlation, use = "pairwise.complete.obs")
print(cor_matrix)
# Convert the correlation matrix to a data frame
cor_matrix_df <- as.data.frame(as.table(cor_matrix))

# Write to CSV
write_csv(cor_matrix_df, "outputs/data/correlation_matrix.csv")

# Convert the matrix to a tibble
cor_tibble <- as_tibble(cor_matrix, rownames = "category_a")

# Convert to long form and filter duplicates
cor_long <- cor_tibble %>% 
  pivot_longer(cols = -category_a, names_to = "category_b", values_to = "correlation_coefficient") %>%
  filter(category_a < category_b)

# Arrange in descending order
cor_long_sorted <- cor_long %>% 
  arrange(desc(correlation_coefficient))

#### Save data ####
write_csv(cor_long_sorted, "outputs/data/analysis_data.csv")

# Generate the summary
summary_stats <- describe(cor_long_sorted$correlation_coefficient)

# Convert the summary to a data frame for easier CSV writing
summary_stats_df <- as.data.frame(summary_stats)

# Write to CSV
write.csv(summary_stats_df, "outputs/data/statistical_summary.csv")