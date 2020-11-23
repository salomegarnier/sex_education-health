
library(stringr)
library(rstanarm)
library(tidyverse)


## How do I make a scatterplot with a regression line once I have my model??

stan_glm(data = full_data,
       formula = matern_mort ~ total + family_planning + curriculum_laws + sex_edu,
       refresh = 0,
       family = "gaussian") 

  # as_tibble() %>%
  # mutate(effect = `(Intercept)` + total + family_planning + curriculum_laws + sex_edu) %>%
  # select(`(Intercept)`, effect) %>%
  # pivot_longer(cols = `(Intercept)`:effect, names_to = "category", values_to = "values") %>%
  # ggplot(aes(values, fill = category)) +
  # geom_histogram()


stan_glm(data = full_data,
         formula = formula,
         refresh = 0,
         family = "gaussian")


# Have two models: one simple model with only a few predictors (with simple plot
# with a regression line) and a second one with all predictors (print out regression table)

# Have public choose the outcome they want to look at

# To choose predictors, use checkbox

# Before next week, set up dropdown for outcome and checkboxes for predictors

# Final app: two models, two plots (one simple and one with all predictors), one
# regression table, and interactive interface to choose outcome + predictors