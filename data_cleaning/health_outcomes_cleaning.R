
# Health outcomes dataset

# Contraceptive Prevalence
contraception <- read_excel("Raw_data/predictors.xlsx", 
                            sheet = "Contraception", skip = 9) %>%
  select(Country, '2008-2018') %>%
  rename(contracept_prev = '2008-2018',
         country = Country)
