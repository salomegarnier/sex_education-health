# Creating the predictors dataset

library(readxl)
library(tidyverse)

# Health expenditures 
health_expenditures <- read_excel("raw_data/health_expenditures.xlsx")%>%
  slice(2:1135) %>%
  
  # I'm getting rid of all the useless rows or those for regions of the world,
  # to only focus on specific countries (since all of my datasets don't have
  # data for different regions).
  
  select(Countries, Indicators, '2017') %>%
  rename(country = Countries, 
         value = '2017') %>%
  
  # In all of these datasets, I will make sure that the countries are listed
  # under the same column name ('country') so that I can later merge them all. I
  # will also rename each column (typically named by the year of the data) to
  # have one column per indicator).
  
  pivot_wider(names_from = Indicators, values_from = value) %>%
  select(!'Health Capital Expenditure (HK) % Gross Domestic Product (GDP)') %>%
  
  # This variable, while I would have been interesting, has almost only NAs, so
  # I won't be using it.
  
  rename(phc_expend = 
           'Primary Health Care (PHC) Expenditure as % Current Health Expenditure (CHE)',
         gov_expend = 
           'General Government Expenditure (GGE) as % Gross Domestic Product (GDP)',
         gdp_capita = 
           'Gross Domestic Product (GDP) per Capita in US$',
         reprod_health_expend = 
           'Domestic General Government Expenditure on Reproductive Health',
         family_plann_expend = 
           'Domestic General Government Expenditure on Contraceptive Management (Family Planning)')

  # I will be doing the same steps for each dataset: skip and/or slice to remove
  # useless rows, select the year I am interested in (often the most recent year
  # or the recent one with the most data), and rename the columns to prepare
  # them for being joined.

# Coefficient of Human Inequality
inequality <- read_excel("raw_data/predictors.xlsx", 
                         sheet = "Inequality", skip = 5) %>%
  select(Country, '2018') %>%
  rename(inequality_coef = '2018',
         country = Country) %>%
  slice(1:165)


# Urbanization
urban <- read_excel("raw_data/predictors.xlsx", 
                         sheet = "Urban", skip = 6) %>%
  select(Country, '2018') %>%
  rename(percent_urban_pop = '2018',
         country = Country) %>%
  slice(1:195)


# Youth Dependency Ratio
youth_ratio <- read_excel("raw_data/predictors.xlsx", 
                         sheet = "Youth Dependency", skip = 9) %>%
  select(Country, '2018') %>%
  rename(youth_depend_ratio = '2018',
         country = Country) %>%
  slice(1:185)


# Median Age
median_age <- read_excel("raw_data/predictors.xlsx", 
                         sheet = "Median Age", skip = 9) %>%
  select(Country, '2020') %>%
  rename(med_age = '2020',
         country = Country) %>%
  slice(1:185)


# Gender Inequality Index
gender_inequality <- read_excel("raw_data/predictors.xlsx", 
                         sheet = "Gender Inequality", skip = 7) %>%
  select(Country, '2018') %>%
  rename(gend_ineq_index = '2018',
         country = Country) %>%
  slice(1:162)


# Human Development Index
hdi <- read_excel("raw_data/predictors.xlsx", 
                            sheet = "HDI", skip = 7) %>%
  select(Country, '2018') %>%
  rename(human_devel_index = '2018',
         country = Country) %>%
  slice(1:189)


# Youth Literacy Rate
literacy <- read_excel("raw_data/literacy_rate_youth.xlsx", 
                                  skip = 3) %>%
  select('Country Name', '2018') %>%
  rename(youth_literacy_rate = '2018',
         country = 'Country Name')


# Population
population <- read_excel("raw_data/population.xlsx") %>%
  select('Country Name', '2019') %>%
  rename(pop_count = '2019',
         country = 'Country Name')


# Poverty Rate (under $3.2 a day)
poverty <- read_excel("raw_data/poverty.xlsx", 
                      skip = 3) %>%
  select('Country Name', '2017') %>%
  rename(poverty_rate = '2017',
         country = 'Country Name')

# School Enrollment for Girls (Secondary School)
school_enrollment <- read_excel("raw_data/school_enrollment.xlsx", 
                                skip = 3) %>%
  select('Country Name', '2018') %>%
  rename(school_enroll_girls = '2018',
         country = 'Country Name')


# State Fragility Index
state_fragility <- read_excel("raw_data/state_fragility.xlsx") %>%
  filter(year == '2018') %>%
  select(country, sfi, region) %>%
  rename(state_frag_index = sfi)


# Polity Score
polity <- read_excel("raw_data/polity.xlsx") %>%
  filter(year == '2018') %>%
  select(country, democ, autoc, polity)


# World Bank Income Group
income_group <- read_excel("raw_data/maternal_mortality.xlsx", 
                                 skip = 6) %>%
  filter(Year == '2017') %>%
  select('World Bank Income Group', 
         Country, 'WHO Region') %>%
  rename(country = Country, 
         income_group = 'World Bank Income Group', 
         region_name = 'WHO Region')


# Joining the datasets
predictors <- full_join(urban, median_age, by = "country") %>%
  full_join(., hdi, by = "country") %>%
  left_join(., polity, by = "country") %>%
  full_join(., inequality, by = "country") %>%
  full_join(., youth_ratio, by = "country") %>%
  full_join(., gender_inequality, by = "country") %>%
  
  # I used full_join for these first datasets because they have a comprehensive
  # list of countries and complement each other. They also only look at
  # countries, and not at different regions of the world (or if they do, these
  # are separated at the bottom, so I could easily slice them off). I used
  # left_join for the following datasets because the regions of the world are
  # intertwined with the countries, so it would have been extremely long to
  # handpick them out. Instead, only the countries in the list created in the
  # previous joins will be matched. All other rows (for regions of the world
  # specifically) will be removed.
  
  left_join(., health_expenditures, by = "country") %>%
  left_join(., state_fragility, by = "country") %>%
  left_join(., population, by = "country") %>%
  left_join(., literacy, by = "country") %>%
  left_join(., poverty, by = "country") %>%
  left_join(., school_enrollment, by = "country") %>%
  left_join(., income_group, by = "country")
  
write_csv(predictors, "shiny_app/predictors_clean.csv")

