

stan_glm(data = full_data,
         formula = input$selected_outcome ~ total + family_planning + curriculum_laws + sex_edu,
         refresh = 0,
         family = "gaussian") %>%
  tbl_regression() %>% 
  as_gt() %>%
  tab_header(title = paste("Regression of", input$selected_outcome),
             subtitle = paste("The Effect of Sexual Education on", input$selected_outcome))



formula <- as.formula(paste(input$selected_outcome_2, "~", paste(input$selected_predictors, collapse =" + ")))



