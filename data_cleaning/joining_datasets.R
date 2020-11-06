
hiv_to_join <- hiv_incidence %>%
  filter(metric == "Rate") %>%
  select(country, val) %>%
  rename(hiv_rate = val)

joined1 <- sex_education %>%
  filter(country != "Africa") %>%
  filter(country != "Asia") %>%
  filter(country != "Americas") %>% 
  filter(country != "Australia and New Zealand") %>%
  filter(country != "Caribbean") %>%
  filter(country != "Central America") %>%
  filter(country != "Central Asia") %>%
  filter(country != "Central and Southern Asia") %>%
  filter(country != "Eastern Africa") %>%
  filter(country != "Eastern and South-Eastern Asia") %>%
  filter(country != "Eastern Asia") %>%
  filter(country != "Eastern Europe") %>%
  filter(country != "Europe") %>%
  filter(country != "Europe and Northern America") %>%
  filter(country != "Landlocked developing countries (LLDCs)") %>%
  filter(country != "Latin America and the Caribbean") %>%
  filter(country != "Least Developed Countries (LDCs)") %>%
  filter(country != "Middle Africa") %>%
  filter(country != "Northern Africa") %>%
  filter(country != "Northern Africa and Western Asia") %>%
  filter(country != "Northern Europe") %>%
  filter(country != "Oceania") %>%
  filter(country != "Small island developing States (SIDS)") %>%
  filter(country != "South America") %>%
  filter(country != "South-Eastern Asia") %>%
  filter(country != "Southern Africa") %>%
  filter(country != "Southern Asia") %>%
  filter(country != "Southern Europe") %>%
  filter(country != "Sub-Saharan Africa") %>%
  filter(country != "Western Africa") %>%
  filter(country != "Western Asia") %>%
  filter(country != "Western Europe") %>%
  filter(country != "World") %>%
  
  full_join(., adolescent_preg, by = "country") %>%
  full_join(., hiv_to_join, by = "country") %>%
  left_join(., predictors, by = "country") %>%
  write_csv("final_proj/fulldataset.csv")


