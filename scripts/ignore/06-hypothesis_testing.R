#### Preamble ####
# Purpose: Find the unique pairwaise category combinations that are
# statistically significant under alpha level of 0.05
# Author: Allyson Cui
# Email: allyson.cui@mail.utoronto.ca
# Date: 1 September 2023
# Prerequisites: None

#### Workspace setup ####
library(tidyverse)

#### Load the Simulated Distribution and analysis data####
setwd("/Users/jiaxincui/Work-Study")
analysis_data <- read_csv("outputs/data/analysis_data.csv")
simulated_distribution <- readRDS("outputs/models/simulated_distribution.rds")

# Function for hypothesis testing
perform_test <- function(observed_coeff, simulated_distribution) {
  # Calculate the empirical p-value based on the simulated distribution
  p_value <- mean(abs(simulated_distribution$correlation_coefficient) >= abs(observed_coeff))
  
  # Declare significance
  significance <- ifelse(p_value < 0.05, TRUE, FALSE)  # Assuming alpha = 0.05
  
  return(data.frame(p_value = p_value, significance = significance))
}

# Run the hypothesis test for each unique pairwise combination
hypothesis_testing <- analysis_data %>%
  rowwise() %>%
  mutate(result = list(perform_test(correlation_coefficient, simulated_distribution))) %>%
  ungroup() %>%
  unnest(cols = c(result))  # This line will automatically add new columns from the list column

# At this point, 'p_value' and 'significance' should now be columns in your data frame. Let's select them.
hypothesis_testing <- hypothesis_testing %>%
  select(category_a, category_b, correlation_coefficient, p_value, significance)

#### Save data ####
write_csv(hypothesis_testing, "outputs/data/hypothesis_testing_result.csv")