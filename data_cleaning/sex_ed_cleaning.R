
# Cleaning the sex_ed dataset

library(readxl)
library(tidyverse)

sex_ed <- read_excel("raw_data/sex_ed.xlsx")

sex_education <- sex_ed %>%
  filter(Indicator == "5.6.2") %>%
  
  # I only want the second indicator, which looks specifically at sexual education. 
  
  select(SeriesCode, GeoAreaName, Value) %>%
  mutate(SeriesCode = str_sub(SeriesCode, 13, 16)) %>%
  
  # I want to reduce the Series Code to just a few symbols that I can later rename easily.

  pivot_wider(names_from = "SeriesCode",values_from = "Value") %>%
  
  # I want a single row per country, with values for each sub-indicator
  
  select(GeoAreaName, E, EC8, ES2, ES3) %>%
  
  # I chose only the sub-indicators I am interested in because there are many
  # (Wisdom!!). I might have to reduce the number of sub-indicators again later.
  
  rename(total = E, 
         curriculum_laws = EC8, 
         family_planning = ES2, 
         sex_edu = ES3, 
         country = GeoAreaName)

write_csv(sex_education, "shiny_app/sex_education_clean.csv")
   

# This dataset might be useful, it only looks at sex education by region.

sex_ed_region <- sex_education %>%
  filter(country %in% c("Africa", "Asia", "Americas", "Europe", 
                        "Latin America and the Caribbean", 
                        "Northern Africa","Oceania",
                        "South-Eastern Asia",
                        "Sub-Saharan Africa",
                        "World"))

