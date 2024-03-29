#### Preamble ####
# Purpose: Download data on 2022 Toronto Business Licenses from OpenData
# Author: Allyson Cui
# Email: allyson.cui@mail.utoronto.ca
# Date: 1 September 2023
# Prerequisites: Have access to OpenData

#### Workspace setup ####
install.packages("opendatatoronto")
install.packages("knitr")
install.packages("dplyr")
install.packages("readr")

library(knitr)
library(janitor)
library(lubridate)
library(opendatatoronto)
library(tidyverse)
library(readr)
library(dplyr)

#### Download data ####
toronto_business_licenses <-
  # https://open.toronto.ca/dataset/municipal-licensing-and-standards-business-licences-and-permits/
  list_package_resources("57b2285f-4f80-45fb-ae3e-41a02c3a137f") |>
  # Within that package, we are interested in the 2022 dataset
  filter(name == "Business licences data.csv") |>
  # Having reduced the dataset to one row we can get the resource
  get_resource()

write_csv(
  x = toronto_business_licenses,
  file = "inputs/data/raw_data.csv"
)