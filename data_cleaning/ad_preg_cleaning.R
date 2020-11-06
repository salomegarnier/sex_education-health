
library(readxl)
library(tidyverse)

ad_pregnancy <- read_excel("Raw_data/adolescent_pregnancy.xlsx")

adolescent_preg <- ad_pregnancy %>%
  drop_na(Country) %>%

# By dropping na, I'm keeping only the first row for each country, i.e., the
# most recent year for which there is data for this country

  rename(year_preg = Year, 
         country = Country, 
         pregnancy_rate = `Adolescent birth rate (per 1000 women aged 15-19 years)`) %>%
  write_csv("final_proj/ad_preg_clean.csv")
