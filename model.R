
library(stringr)
library(rstanarm)
library(tidyverse)


stan_glm(data = full_data,
       formula = matern_mort ~ total + family_planning + polity + percent_urban_pop,
       refresh = 0,
       family = "gaussian")


# Have two models: one simple model with only a few predictors (with simple plot
# with a regression line) and a second one with all predictors (print out regression table)

# Have public choose the outcome they want to look at

# To choose predictors, use checkbox

# Before next week, set up dropdown for outcome and checkboxes for predictors

# Final app: two models, two plots (one simple and one with all predictors), one
# regression table, and interactive interface to choose outcome + predictors