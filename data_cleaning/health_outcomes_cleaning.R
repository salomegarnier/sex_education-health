
# Creating the Health Outcomes Dataset

library(readr)
library(readxl)
  
  # I essentially do the same thing that I did to create the predictors dataset:
  # each variable comes from a different dataset. I import the dataset, skip
  # and/or slice to remove useless rows, select the year I am interested in
  # (often the most recent year or the recent one with the most data), and
  # rename the columns to prepare them for being joined. Some require being
  # pivoted to match the format of the final dataset.

# Contraceptive Prevalence
contraception <- read_excel("Raw_data/predictors.xlsx", 
                            sheet = "Contraception", 
                            skip = 9) %>%
  select(Country, '2008-2018') %>%
  rename(contracept_prev = '2008-2018',
         country = Country) %>%
  slice(1:164)


# Life Expectancy
life_expectancy <- read_excel("Raw_data/life_expectancy.xlsx") %>%
  select('Series Name', 'Country Name', '2020 [YR2020]') %>%
  slice(1:774) %>%
  pivot_wider(names_from = 'Series Name', 
              values_from = '2020 [YR2020]', 
              id_cols = 'Country Name', 
              values_fn = as.numeric) %>%
  rename(total_life_exp = 'Life expectancy at birth, total (years)',
         male_life_exp = 'Life expectancy at birth, male (years)',
         female_life_exp = 'Life expectancy at birth, female (years)',
         country = 'Country Name') %>%
  select(country, total_life_exp, female_life_exp) %>%
  drop_na()


# Maternal Mortality
maternal_mortality <- read_excel("Raw_data/maternal_mortality.xlsx", 
                                 skip = 6) %>%
  filter(Year == '2017') %>%
  select(Country, Value) %>%
  rename(country = Country, matern_mort = Value)


# Young people (15-24) newly infected with HIV
young_people_hiv <- read_excel("Raw_data/young_people_hiv.xlsx", 
                               skip = 3) %>%
  select('Country Name', '2019') %>%
  rename(country = 'Country Name', youth_hiv = '2019')


# Health Outcomes Full Dataset
health_outcomes <- full_join(hiv_to_join, adolescent_preg, 
                             by = "country") %>%
  
  # hiv_to_join and adolescent_preg were created in an earlier R script.
  
  full_join(., contraception, 
            by = "country") %>%
  
  # Again, I chose to left_join or full_join depending on the country list (if
  # the dataset had countries that were missing vs if it included regions of the
  # world that I did not want to include).
  
  left_join(., young_people_hiv, 
            by = "country") %>%
  full_join(., maternal_mortality, 
            by = "country") %>%
  left_join(., life_expectancy, 
            by = "country") %>%
  select(!year_preg) %>%
  drop_na(female_life_exp) %>%
  
  # When looking at the dataset, I saw that the countries that had no values in
  # female_life_exp were also NA in most other columns, so I removed those rows.
  
  write_csv("final_proj/health_outcomes_clean.csv")
         