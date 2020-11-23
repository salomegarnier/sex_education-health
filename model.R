
library(stringr)
library(rstanarm)
library(tidyverse)


## Plot for model 1

model_1 <- stan_glm(data = full_data,
       formula = matern_mort ~ total + family_planning + curriculum_laws + sex_edu,
       refresh = 0,
       family = "gaussian")

new_obs <- tibble(total = seq(0, 100, by = .1),
       family_planning = mean(full_data$family_planning, na.rm = TRUE),
       curriculum_laws = mean(full_data$curriculum_laws, na.rm = TRUE),
       sex_edu = mean(full_data$sex_edu, na.rm= TRUE))

predict_1 <- predict(model_1, newdata = new_obs, se.fit = TRUE) 

tibble(total = seq(0, 100, .1),
       prediction = predict_1$fit,
       se = predict_1$se.fit) %>%
  ggplot(aes(total, prediction)) +
  geom_line() +
  geom_errorbar(aes(ymin = prediction - 1.96*se, ymax = prediction + 1.96*se),
                alpha = 0.2, color = "blue")

  # Potentially make regression tables ahead of time to avoid long wait time


# Model 2

stan_glm(data = full_data,
         formula = formula,
         refresh = 0,
         family = "gaussian")

# Custom formula

formula <- as.formula(paste(input$selected_outcome_2, "~", paste(input$selected_predictors, collapse =" + ")))



# Have two models: one simple model with only a few predictors (with simple plot
# with a regression line and regression table) and a second one with all predictors (print out regression table)

# Final app: two models (one simple and one with all predictors), one plots, two
# regression tables, and interactive interface to choose outcome + predictors
