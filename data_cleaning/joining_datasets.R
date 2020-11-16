
# Joining all datasets together for the full dataset

  # When joining these datasets, I realized that the country names were often
  # spelled differently. For example, some wrote Slovakia and others had Slovak
  # Republic. Some included the words "kingdom" or "republic" or even "the"
  # while others didn't. North Korea and South Korea, Congo and the DRC, even
  # the UK and the US had many different appellations. I had to go through these
  # datasets manually to change the names so that the final dataset was unified.
  # I did this through Excel because it was quicker and easier to just find and
  # replace the names of countries.


full_data <- sex_education %>%
  
  # I did this filtering before realizing I could left_join this dataset to the
  # other ones that only have country names. I won't delete it now that it's
  # done, but it was ultimately a waste of time.
  
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
  
  # Here is where I add the two other datasets I have created. I full_join
  # health outcomes because some countries are missing in the sex_ed dataset,
  # but only left_join predictors so that I have a relatively uniform dataset.
  
  full_join(., health_outcomes, by = "country") %>%
  left_join(., predictors, by = "country") %>%
  mutate(across(.cols = total:school_enroll_girls, 
                as.numeric)) %>%
  
  # I want to make sure all my outcomes and predictors are numeric, so that I
  # can use them in my model.
  
  write_csv("final_proj/fulldataset.csv")


