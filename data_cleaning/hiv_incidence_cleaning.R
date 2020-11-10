
library(readxl)
library(tidyverse)

# Cleaning the hiv_incidence data 

hiv <- read_csv("Raw_data/HIV_incidence/IHME-GBD_2017_DATA-5b83230d-1.csv", 
                col_types = cols(
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

# This dataset might be helpful in the future – it contains different metrics
# for HIV.

hiv_to_join <- hiv_incidence %>%
  filter(metric == "Rate") %>%
  select(country, val) %>%
  rename(hiv_rate = val)

# Here, I am choosing a single metric (the incidence rate) to include in the
# full dataset. This is the best metric to compare between countries. I will use
# this new tibble when I join all datasets.