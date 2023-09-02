#### Preamble ####
# Purpose: To calculate the theoretical distribution of correlation coefficients
# under the assumption that occurrences follow a Poisson distribution with a 
# mean of 3.
# Author: Allyson Cui
# Date: 1 September 2023
# Pre-requisites: None

#### Workspace setup ####
library(tidyverse)

#### Simulate data ####
set.seed(123)

simulate_data <- function(n_simulations = 10000, n_samples = 100, lambda = 3) {
  correlation_coeffs <- numeric(n_simulations)
  
  for (i in seq_len(n_simulations)) {
    data_a <- rpois(n_samples, lambda)
    data_b <- rpois(n_samples, lambda)
    
    correlation_coeffs[i] <- cor(data_a, data_b)
  }
  
  return(data.frame(correlation_coefficient = correlation_coeffs))
}

# Simulate and save the theoretical distribution
simulated_distribution <- simulate_data()
saveRDS(simulated_distribution, file = "outputs/models/simulated_distribution.rds")
