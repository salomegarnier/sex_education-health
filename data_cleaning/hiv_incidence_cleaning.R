
library(readxl)
library(tidyverse)

hiv <- read_csv("Raw_data/HIV_incidence/IHME-GBD_2017_DATA-5b83230d-1.csv", col_types = cols(
  measure = col_character(),
  location = col_character(),
  sex = col_character(),
  age = col_character(),
  cause = col_character(),
  metric = col_character(),
  year = col_double(),
  val = col_double(),
  upper = col_double(),
  lower = col_double()))

hiv_incidence <- hiv %>%
  select(location, metric, year, val) %>%
  rename(country = location) %>%
  write_csv("final_proj/hiv_incidence_clean.csv")
