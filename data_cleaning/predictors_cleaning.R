# Creating the predictors dataset

library(readxl)
library(tidyverse)

# Health expenditures 
health_expenditures <- read_excel("Raw_data/health_expenditures.xlsx")%>%
  slice(2:1135) %>%
  select(Countries, Indicators, '2017') %>%
  rename(country = Countries, 
         value = '2017') %>%
  pivot_wider(names_from = Indicators, values_from = value) %>%
  select(!'Health Capital Expenditure (HK) % Gross Domestic Product (GDP)') %>%
  rename(phc_expend = 'Primary Health Care (PHC) Expenditure as % Current Health Expenditure (CHE)',
         gov_expend = 'General Government Expenditure (GGE) as % Gross Domestic Product (GDP)',
         gdp_capita = 'Gross Domestic Product (GDP) per Capita in US$',
         reprod_health_expend = 'Domestic General Government Expenditure on Reproductive Health',
         family_plann_expend = 'Domestic General Government Expenditure on Contraceptive Management (Family Planning)')

# Coefficient of Human Inequality
inequality <- read_excel("Raw_data/predictors.xlsx", 
                         sheet = "Inequality", skip = 5) %>%
  select(Country, '2018') %>%
  rename(inequality_coef = '2018',
         country = Country) %>%
  slice(1:165)

# Urbanization
urban <- read_excel("Raw_data/predictors.xlsx", 
                         sheet = "Urban", skip = 6) %>%
  select(Country, '2018') %>%
  rename(percent_urban_pop = '2018',
         country = Country) %>%
  slice(1:195)

# Youth Dependency Ratio
youth_ratio <- read_excel("Raw_data/predictors.xlsx", 
                         sheet = "Youth Dependency", skip = 9) %>%
  select(Country, '2018') %>%
  rename(youth_depend_ratio = '2018',
         country = Country) %>%
  slice(1:185)

# Median Age
median_age <- read_excel("Raw_data/predictors.xlsx", 
                         sheet = "Median Age", skip = 9) %>%
  select(Country, '2020') %>%
  rename(med_age = '2020',
         country = Country) %>%
  slice(1:185)

# Gender Inequality Index
gender_inequality <- read_excel("Raw_data/predictors.xlsx", 
                         sheet = "Gender Inequality", skip = 7) %>%
  select(Country, '2018') %>%
  rename(gend_ineq_index = '2018',
         country = Country) %>%
  slice(1:162)

# Human Development Index
hdi <- read_excel("Raw_data/predictors.xlsx", 
                            sheet = "HDI", skip = 7) %>%
  select(Country, '2018') %>%
  rename(human_devel_index = '2018',
         country = Country) %>%
  slice(1:189)

# Youth Literacy Rate
literacy <- read_excel("Raw_data/literacy_rate_youth.xlsx", 
                                  skip = 3) %>%
  select('Country Name', '2018') %>%
  rename(youth_literacy_rate = '2018',
         country = 'Country Name')

# Population
population <- read_excel("Raw_data/population.xlsx") %>%
  select('Country Name', '2019') %>%
  rename(pop_count = '2019',
         country = 'Country Name')

# Poverty Rate (under $3.2 a day)
poverty <- read_excel("Raw_data/poverty.xlsx", 
                      skip = 3) %>%
  select('Country Name', '2017') %>%
  rename(poverty_rate = '2017',
         country = 'Country Name')

# School Enrollment for Girls (Secondary School)
school_enrollment <- read_excel("Raw_data/school_enrollment.xlsx", 
                                skip = 3) %>%
  select('Country Name', '2018') %>%
  rename(school_enroll_girls = '2018',
         country = 'Country Name')

# State Fragility Index
state_fragility <- read_excel("Raw_data/state_fragility.xlsx") %>%
  filter(year == '2018') %>%
  select(country, sfi) %>%
  rename(state_frag_index = sfi)

# Polity Score
polity <- read_excel("Raw_data/polity.xlsx") %>%
  filter(year == '2018') %>%
  select(country, democ, autoc, polity)

# World Bank Income Group
income_group <- read_excel("Raw_data/maternal_mortality.xlsx", 
                                 skip = 6) %>%
  filter(Year == '2017') %>%
  select('World Bank Income Group', Country, 'WHO Region') %>%
  rename(country = Country, income_group = 'World Bank Income Group', region = 'WHO Region')

# Joining the datasets
predictors <- full_join(urban, median_age, by = "country") %>%
  full_join(., hdi, by = "country") %>%
  left_join(., polity, by = "country") %>%
  full_join(., inequality, by = "country") %>%
  full_join(., youth_ratio, by = "country") %>%
  full_join(., gender_inequality, by = "country") %>%
  left_join(., health_expenditures, by = "country") %>%
  left_join(., state_fragility, by = "country") %>%
  left_join(., population, by = "country") %>%
  left_join(., literacy, by = "country") %>%
  left_join(., poverty, by = "country") %>%
  left_join(., school_enrollment, by = "country") %>%
  left_join(., income_group, by = "country")
  
write_csv(predictors, "final_proj/predictors_clean.csv")

