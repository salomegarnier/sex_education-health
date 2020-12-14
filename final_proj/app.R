
library(shiny)
library(tidyverse)
library(shiny)
library(fec16)
library(ggtext)
library(mdthemes)
library(rstanarm)
library(gtsummary)
library(gt)
library(broom.mixed)
library(DT)
library(shinythemes)

# These are the datasets that I will be using in this project, with some changes
# that I made after having created/prepared them in a separate R document. For
# example, I decided to rename some of the columns and delete others to fit with
# the aesthetics I wanted for the website. I also mutated columns to numeric when
# relevant, for both aesthetic and modeling purposes.

sex_ed <- read_csv("sex_education_clean.csv") %>%
  rename(curriculum = curriculum_laws,
         sex_edu_laws = sex_edu)

full_data <- read_csv("fulldataset.csv") %>%
  rename(curriculum = curriculum_laws,
         sex_edu_laws = sex_edu) %>%
  select(!hiv_rate)

predictors <- read_csv("predictors_clean.csv") %>%
  mutate(across(.cols = percent_urban_pop:school_enroll_girls, 
                as.numeric))

health_outcomes <- read_csv("health_outcomes_clean.csv") %>%
  mutate(across(.cols = hiv_rate:female_life_exp, 
                as.numeric)) %>%
  select(!hiv_rate)

# Here, I create objects that I'll need later on. The first is just a tibble
# combining column names and labels. The second will be useful for making
# predictions using my model.

outcome_table <- c("Teen Pregnancy Rate" = "pregnancy_rate", 
                   "Contraception Prevalence" = "contracept_prev", 
                   "Youth HIV Rate" = "youth_hiv", 
                   "Maternal Mortality" = "matern_mort", 
                   "Total Life Expectancy" = "total_life_exp", 
                   "Female Life Expectancy" = "female_life_exp")

new_obs <- tibble(total = seq(0, 100, by = .1),
                  family_planning = mean(full_data$family_planning, na.rm = TRUE),
                  curriculum = mean(full_data$curriculum, na.rm = TRUE),
                  sex_edu_laws = mean(full_data$sex_edu_laws, na.rm= TRUE))

######################################################################################
######################################################################################
# 

# User interface

ui <- fluidPage(theme = shinytheme("sandstone"),
  
  navbarPage("Sex Education and Women and Girls' Health",

             # Panel 1
             # This is my introductory panel, which focuses mostly on the SDGs.
             # I explain what they are, which one I am looking at specifically,
             # how the scores are calculated, and more. I included a graph with 
             # scores for each country or region. 
             
             tabPanel("SDG 5.6.2.: Sexual Education",
                      h1("How does each country rank in sex education?"),
                      p("Based on the UN Sustainable Development Goals", 
                        style = "font-size:20px;"),
                      br(),
                      
                      # Main Panel
                      mainPanel(
                        selectInput(inputId = "selected_country",
                                    label = "Choose a country or region",
                                    choices = sex_ed$country,
                                    selected = "Africa"),
                        
                        plotOutput("country_plot"),
                        br(),
                        p("Within target 5.6., I chose to focus specifically on
                          indicator 5.6.2: the extent to which countries have 
                          laws and regulations that guarantee women aged 15-49", 
                          strong("access to sexual and reproductive health care, 
                                 information and education."), 
                          "Each country gets a score for this indicator, 
                          as well as for other specific measures related to this goal."), 
                        p("The scores are reported as", 
                          a(href = "https://unstats.un.org/sdgs/files/meetings/
                            webex-6sep2018/6.%20UNFPA%205.6.2%20Presentation.pdf", 
                            "percentage scores"), 
                          "between 0 and 100, reflecting", 
                          strong("the extent to which such laws and regulations exist."), 
                          "Beyond the total score, I have to chosen to include 1) to what 
                          extent Comprehensive Sexual Education (CSE) is present in the law, 
                          2) to what extent CSE is present in school curriculums, and 
                          3) access to family planning and contraception.")),
                      
                      # Side Panel
                      sidebarPanel(
                        h4("SDG #5: Achieve gender equality 
                           and empower all women and girls"),
                        h5("Target 5.6.2: Having laws and regulations 
                           that guarantee full and equal access to women 
                           and men aged 15 years and older to sexual and 
                           reproductive health care, information and education")),
                      br(),
                      h2("About the Data"),
                      p("The United Nations'", 
                        a(href="https://sdgs.un.org/goals", 
                          "Sustainable Develoment Goals"), 
                        "(SDGs) were adopted in 2015 as a global 
                        blueprint for peace and prosperity. As a unit, 
                        they recognize the strategic importance of 
                        ending poverty through improving health and 
                        education, as well as reducing inequalities. 
                        They also reaffirm the UN's dedication to tackling 
                        climate change and preserving the environment."),
                      p("SDG #5 is to", 
                        strong("achieve gender equality and empower all women and girls."), 
                        "Improving women's and girls' health is a key step towards that goal. 
                        Many targets within this goal aim to improve health; I am interested 
                        in understanding how one target in particular – target 5.6: Ensure 
                        universal access to sexual and reproductive health and reproductive 
                        rights – can ameliorate women and girls' health around the world.")
             ),
             
             # Panel 2
             # I use this panel to show my data. It mostly consists of two dt
             # tables.
             
             tabPanel("Data",
                      h3("How Well Does Sexual Education Predict Health 
                         Outcomes Compared with Other Indicators?"),
                      p("In the following tab, I will be making models to 
                        determine how well sexual education scores can", 
                        strong("predict a selection of health outcomes.")), 
                      p("For this purpose, I collected data on health outcomes 
                        per country, as well as on various predictors that may have a", 
                        strong("stronger predictive value on the outcomes.")),
                      p("You can take a look at all the data here!"),
                      br(),
                      h4("Health Outcomes"),
                      dataTableOutput('health'),
                      p(em("Sources: UN Stats, World Health Organization, 
                           Institute for Health Metrics and Evaluation, 
                           World Development Indicators, 
                           United Nations Development Programme", 
                           style = "font-size:10px;")),
                      h4("Predictors"),
                      dataTableOutput('predictors'),
                      p(em("Sources: UN Stats, World Development Indicators, 
                           Center for Systemic Peace, UN Population Division, 
                           United Nations Development Programme", 
                           style = "font-size:10px;")),
                      p("* The Democracy and Autocracy Scores are calculated 
                        on a scale from 0 to 10. The Polity Score ranges from 
                        -10 to 10.", 
                        style = "font-size:10px;")

             ),
             
             # Panel 3
             # Here are my two models, including a plot for the first one and a
             # regression table for both.
             
             tabPanel("Models",
                      mainPanel(
                         h3("Model 1: Simple Regression"),
                         p("This first model regresses a chosen 
                           health outcome on all four SDG scores."),
                         p("Choose an outcome below to visualize 
                           its predicted variation as access to sexual 
                           education increases."),
                         selectInput(inputId = "selected_outcome",
                           label = "Choose a health outcome:",
                           choices = c("Teen Pregnancy Rate" = "pregnancy_rate", 
                                       "Contraception Prevalence" = "contracept_prev", 
                                       "Youth HIV Rate" = "youth_hiv", 
                                       "Maternal Mortality" = "matern_mort", 
                                       "Total Life Expectancy" = "total_life_exp", 
                                       "Female Life Expectancy" = "female_life_exp"),
                           selected = "matern_mort"),
                         plotOutput("model_1_plot"),
                         br(),
                         
                         p("This plot shows us the predicted change in our chosen health 
                           outcome as the SDG 5.6.2. Total score increases. All other scores are
                           held constant at their median value."),
                         p("In addition to a regression line, this plot also prints out the 
                         standard error for our predictions. As you can see, it is very wide! 
                         In the regression table, you can find the", strong("95% confidence 
                         interval"), "for each coefficient. These depict the ", 
                           strong("incertainty"), "around each value. Large confidence 
                           intervals in this model suggest that the results are not very precise. 
                           This is why, in the next model, we will be including other indicators, 
                           in an attempt to determine", strong("more precise predictors.")),
                         br(),
                         br(),
                         br(),
                         
                         h3("Model 2: Predictors"),
                         selectInput(inputId = "selected_outcome2",
                                     label = "Choose a health outcome:",
                                     choices = c("Teen Pregnancy Rate" = 
                                                   "pregnancy_rate", 
                                                 "Contraception Prevalence" = 
                                                   "contracept_prev", 
                                                 "Youth HIV Rate" = 
                                                   "youth_hiv", 
                                                 "Maternal Mortality" = 
                                                   "matern_mort", 
                                                 "Total Life Expectancy" = 
                                                   "total_life_exp", 
                                                 "Female Life Expectancy" = 
                                                   "female_life_exp"),
                                     selected = "matern_mort"),
                         p("For this next model, we will add a number of related indicators 
                          to our model to establish which ones most strongly predict a 
                           country’s health outcomes. By examining the coefficients for
                           each predictor, we will compare the magnitude and precision
                           with which they can accurately predict our chosen  outcome,
                           using the same logic as above. Select any variables that you think  
                           may be strong predictors of health outcomes for women and girls."),
                         selectInput(inputId = "selected_predictors",
                                     label = "Choose your predictors:",
                                     choices = c("Urbanization Rate" = 
                                                   "percent_urban_pop",
                                                 "Median Age" = 
                                                   "med_age",
                                                 "HDI" = 
                                                   "human_devel_index",
                                                 "Inequality" = 
                                                   "inequality_coef",
                                                 "GDP per Capita" = 
                                                   "gdp_capita",
                                                 "School Enrollment Rate (girls)" = 
                                                   "school_enroll_girls",
                                                 "Gender Inequality Index" = 
                                                   "gend_ineq_index",
                                                 "Youth Dependency Ratio" = 
                                                   "youth_depend_ratio",
                                                 "Primary Healthcare Expenditures" = 
                                                   "phc_expend",
                                                 "Total Government Expenditures" = 
                                                   "gov_expend",
                                                 "Expenditures on Reproductive Health" = 
                                                   "reprod_health_expend",
                                                 "Expenditures on Family Planning" = 
                                                   "family_plann_expend",
                                                 "State Fragility Index" = 
                                                   "state_frag_index",
                                                 "Democracy Score" = 
                                                   "democ",
                                                 "Autocracy Score" = 
                                                   "autoc",
                                                 "Polity Score" = 
                                                   "polity",
                                                 "World Region" = 
                                                   "region",
                                                 "Total Population" = 
                                                   "pop_count",
                                                 "Youth Literacy Rate" = 
                                                   "youth_literacy_rate",
                                                 "Poverty Rate" = 
                                                   "poverty_rate",
                                                 "Income Group" = 
                                                   "income_group"),
                                     multiple = TRUE,
                                     selected = c("school_enroll_girls", 
                                                  "inequality_coef")),
                         gt_output("model2"),
                         br(),
                         br()
                         ),
                        br(),
                        br(),
                        withMathJax(),
                      
                      # withMathJax() will allow me to write my function in LaTex.
                      
                        gt_output("model1"),
                        p('$$ outcome_i = \\beta_0 + \\beta_1total_i + 
                          \\beta_2plan_i + \\beta_3curric_i +
                          \\beta_4laws_i + \\epsilon_i $$'),
                          p("The coefficients in this regression table 
                          (under the Beta column) represent the", 
                          strong("predicted change in the chosen health outcome 
                          for a unit increase of the value of each SDG score,"), 
                          "holding all other scores constant at zero. A unit increase
                          here is an increase of 1 in the specific SDG score, which ranges 
                          from 0 to 100. In other words, these coefficients represent the 
                          difference between two hypothetical groups: one where all
                          scores are held constant at zero (this is our Intercept value), 
                          and another where", strong("just one of the scores increases."),
                          "For example, the predicted maternal mortality rate is 492 (the 
                          Intercept of our model) for a country with scores of zero on each 
                          indicator, and it is predicted to", em("decrease by -7.5 for each unit
                          increase in the total SDG score."), "This works for other scores too:", 
                          em("each increase of one"), "on the family_planning or curriculum score 
                          is estimated to", em("increase or decrease the value of the chosen health 
                          indicator"), "by the coefficient listed in the Beta column."),
                        p("The mathematical formula above depicts this relationship: each 
                          coefficient in our table represents the median of the posterior 
                          for the Beta values in our above formula. The Intercept 
                          (\\(\\beta_0\\)) is the", strong("value of the health outcome
                          when all predictors are held constant"), "at zero. When multiplied
                          by the value of each corresponding SDG score, coefficients  
                          can be added up to the Intercept (\\(\\beta_0\\)) to calculate 
                          \\(outcome_i\\), the", strong("predicted value of the health outcome"),  
                          "based on SDG scores. \\(\\epsilon_i\\) is the error term; it accounts 
                          for the uncertainty of our predictions."),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),    
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        h4("Interpretation:"),
                        p("Looking at this regression table, the Total SDG Score for 
                        sexual education seems to be a", 
                          strong("pretty poor predictor of health outcomes for women 
                                 and girls."),
                          "The ", em("95% Confidence Interval"), 
                          "is often huge, and the coefficient is", 
                          em("relatively insignificant"), 
                          "compared to other predictors. Inequality, for example, seems 
                          to be a much stronger predictor for many health outcomes."),
                        p("These results suggest that", 
                          strong("sexual education alone may not be sufficient for 
                          improving health outcomes"), 
                          "for women and girls. The predictors with
                          the highest coefficients and lowest uncertainty, 
                          such as School Enrollment Rates for Girls or Gender 
                          Inequality Index, are those that impact health the most. 
                          These results suggest that such issues should be addressed
                          in priority, before sexual education laws can be effective.")
             ),
             
             # Panel 4
             
              tabPanel("About",
                mainPanel(
                 h3("About the Project"),
                 h4("Summary"),
                 p("This project focuses on sexual education around the world, 
                   and its relation to health outcomes for women and girls. 
                   I use data from the", strong("sustainable development goals,"), 
                   "which calculate scores for each country based on the extent to 
                   which sexual education is available. For this analysis, 
                   I was interested in determining how well a country’s sexual 
                   education profile could predict health outcomes for women and girls, 
                   and found that the relationship, while positive, was not very strong. 
                   Other indicators, such as inequality and school enrollment for girls, 
                   seem to be better predictors of the outcomes I observed."),
                 h4("Motivation"),
                 p("The idea for this project stems from my interest in the", 
                   strong("social determinants of health."), "Beyond medical factors, there 
                   are so many social, cultural, and economic factors that impact health. 
                   My project serves to show that a multitude of factors are at play, and
                   that no single factor can improve health on its own. Each of the predictors
                   I included in my models, in their own way, have an impact on health. 
                   Inequality and education are obvious ones, but less related factors
                   (e.g. level of democracy) as well as more targeted factors (e.g. 
                   sexual education laws) can play a significant role in improving
                   health outcomes. Solving major health challenges will require 
                   adressing all of these public policy areas."),
                 h4("Data"),
                 p("My data comes from UN Stats, the World Development Indicators,
                 the Center for Systemic Peace, the UN Population Division,
                 the UN Development Programme, the World Health Organization,
                 and the Institute for Health Metrics and Evaluation. The dates
                 range from 2017 to 2020, or the most recent year. I carefully
                 selected datasets that I considered useful for my project, 
                 and spent a lot of time preparing and merging each dataset to 
                 have a single, coherent dataset. This process included making
                 sure the country names were the same in each dataset (which was
                 rarely the case), so that I could merge them seamlessly. The 
                 original datasets can be found in the Raw_data folder of my", 
                 a(href="https://github.com/salomegarnier/Final_Project.git",
                   "GitHub repository."), 
                 "In the repo, you can also find my Shiny app and data wrangling code.")),
                br(),
                br(),
                br(),
                sidebarPanel(img(src = "IMG_4260.jpg", height = 200),
                             h3("About Me"),
                             h4("Salomé Garnier, Harvard '22"),
                p("I am an undergraduate student studying Government on the 
                  Public Policy Track, with a secondary in Global Health and  
                  Health Policy. I am interested in using data to understand 
                  the relationship between social determinants and health."))
  )))

# Server

  server <- function(input, output, session) {
    
    # Plot 1
    
    output$country_plot <- renderPlot({
      
      sex_ed %>%
        
        pivot_longer(names_to = "sdg", 
                     values_to = "value", 
                     cols = total:sex_edu_laws) %>%
        filter(country == input$selected_country) %>%
        
        ggplot(aes(x = sdg, y = value)) + 
        geom_col(aes(fill = sdg)) +
        geom_text(aes(x = sdg, 
                      label = value),
                  size = 10,
                  y = 10,
                  color = "#585563") +
        
        # I added a geom_text here because I wanted the user to differentiate a
        # value of 0 with an N/A value, which are somewhat common in the
        # dataset. While an N/A is not very useful, a score of 0 is very
        # telling.
        
        theme_classic() +
        labs(title = paste("Sexual Education in", input$selected_country),
             x = "Sustainable Development Goal Indicator",
             y = "Score (out of 100)") +
        md_theme_classic() +
        
        # This function comes from a package that I hadn't used before. It
        # requires markdown text instead of normal text, and allowed me to bold
        # some words in my legend. It required changing the element_text()
        # function to an element_markdown() function, but otherwise everything
        # else works the same.
        
        theme(axis.text.x = element_markdown(size = 13), 
              axis.text.y = element_markdown(size = 13),
              plot.title = element_markdown(size = 20, 
                                        face = "bold"), 
              axis.title.x = element_markdown(size = 16, 
                                              margin = margin(t = 20)),
              axis.title.y = element_markdown(size = 16),
              legend.position = "bottom",
              legend.text = element_markdown(size = 12),
              legend.title = element_blank()) +
        scale_x_discrete(breaks = c("curriculum", "family_planning", 
                                    "sex_edu_laws", "total"), 
                         labels = c("Curriculum", "Family Planning", 
                                    "Sex Education Laws", "SDG Total Score")) +
        scale_fill_manual(breaks = c("curriculum", "family_planning", 
                                     "sex_edu_laws", "total"),
                            labels = c("**Curriculum:** extent to <br>which 
                                       comprehensive <br>sexual education is 
                                       <br>included in school curricula",
                                       "**Family Planning:** <br>access to family 
                                       <br>planning resources <br>and contraception",
                                       "**Sex Education Laws:** extent <br>to which 
                                       comprehensive <br>sexual education is 
                                       <br>included in country laws",
                                       "**SDG Total Score:** extent to which <br>countries
                                       have laws and regulations <br>that guarantee access 
                                       to sexual and <br>reproductive health care, 
                                       information <br>and education"),
                          
                          # I chose to include the descriptions of each score in
                          # the legend, so that users can easily understand what
                          # each score represents.
                          
                          values = c("#C9B1BD", "#7EBDC2", "#F3DFA2", "#8C7DA1")) +
        ylim(c(0, 100))
      
      # Setting the dimensions of the y-axis to 0-100 was essential to be able
      # to compare different countries. Otherwise, the y-axis would vary with
      # each country, making a visual comparison impossible.
      
    })
    
    # Tables 
    
    output$predictors <- renderDataTable(predictors %>% mutate(across(is.numeric, 
                                                                      ~ round(., 2))), 
                                         
                                         # To make my tables look nicer, I made
                                         # sure the values were numeric instead
                                         # of a character string, and rounded
                                         # them up to only show two digits. That
                                         # way, I avoided unnecessary strings of
                                         # 8 or 9 digits.
                                         
                                         options = list(pageLength = 5,
                                                        scrollX = T),
                                         
                                         # These and the following are just
                                         # aesthetic arguments. I wanted the
                                         # tables to scroll instead of having to
                                         # scroll the entire page, and I wanted
                                         # only the first five observations to
                                         # show. Then, I changed other features
                                         # with the class argument.
                                         
                                         class = "compact hover order-column nowrap",
                                         colnames = c("Country",
                                                      "Urbanization Rate (%)",
                                                      "Median Age",
                                                      "HDI",
                                                      "Democracy Score*",
                                                      "Autocracy Score*",
                                                      "Polity Score*",
                                                      "Gini Coefficient",
                                                      "Youth Dependency Ratio",
                                                      "Gender Inequality Index",
                                                      "Primary Healthcare Expenditures",
                                                      "Total Government Expenditures",
                                                      "GDP per Capita",
                                                      "Expenditures on Reproductive Health",
                                                      "Expenditures on Family Planning",
                                                      "State Fragility Index",
                                                      "World Region",
                                                      "Total Population",
                                                      "Youth Literacy Rate",
                                                      "Poverty Rate",
                                                      "School Enrollment Rate (girls)",
                                                      "Income Group",
                                                      "Region Name"))
    
    output$health <- renderDataTable(health_outcomes %>% mutate(across(is.numeric, 
                                                                       ~ round(., 2))), 
                                     options = list(pageLength = 5,
                                                    scrollX = T), 
                                     class = "compact hover order-column",
                                     colnames = c("Country", 
                                                  "Teen Pregnancy Rate", 
                                                  "Contraception Prevalence", 
                                                  "Youth HIV Rate", 
                                                  "Maternal Mortality", 
                                                  "Total Life Expectancy", 
                                                  "Female Life Expectancy"))
    
    
  # Model 1
    
    # This first model is reactive, so that it changes with each selected
    # outcome.
    
    model_1 <- reactive({
                 stan_glm(data = full_data,
                          formula = get(input$selected_outcome) 
                          ~ total + family_planning 
                          + curriculum + sex_edu_laws,
                          refresh = 0,
                          family = "gaussian")})
    
    # Render GT Table
    
    output$model1 <- render_gt(
      model_1() %>%
        tbl_regression(intercept = TRUE) %>%
        
        # I included the intercept in the table because it would be easier to
        # interpret.
        
        as_gt() %>%
        tab_header(title = paste("Regression of", 
                                 names(outcome_table)
                                 [outcome_table == input$selected_outcome]),
                   
                   # This is where the outcome_table that I created earlier
                   # comes in handy. I don't want to column name to appear, I
                   # want a clean title associated with that column name.
                   
                   subtitle = paste("Predicted Changes in", 
                                    names(outcome_table)
                                    [outcome_table == input$selected_outcome],
                                    "as SDG scores increase")))
    
        # Predicting a health outcome based on SDG total score
    
    predict_1 <- reactive({predict(model_1(), 
                                   newdata = new_obs, 
                                   se.fit = TRUE)})
    
               # Here, I am predicting the value of the selected health outcome
               # based on the value of total, keeping all other predictors
               # constant at their mean. I am using a tibble that I created above.
               # I include the standard error to make it appear on my plot.
    
    
        # Plot
    
     output$model_1_plot <- renderPlot({
      tibble(total = seq(0, 100, .1),
             prediction = predict_1()$fit,
             se = predict_1()$se.fit) %>%
         
         # This tibble includes the predictions from above and their standard
         # error.
         
        ggplot(aes(total, prediction)) +
        geom_line(size = 1.5, 
                  color = "#38618C", 
                  alpha = 2) +
        geom_errorbar(aes(ymin = prediction - 1.96*se, 
                          ymax = prediction + 1.96*se),
                      alpha = 0.2, 
                      color = "#ACECF7") +
         
         # This error bar shows how much of a variation there is. 
         
         labs(title = paste("Predictions of", 
                            names(outcome_table)
                            [outcome_table == input$selected_outcome],
                            "based on Sexual Education"),
              subtitle = paste("Predicted change in", 
                               names(outcome_table)
                               [outcome_table == input$selected_outcome], 
                               "as a country's SDG 5.6.2. total score increases"),
              x = "SDG 5.6.2. total score",
              y = paste("Predicted", 
                        names(outcome_table)
                        [outcome_table == input$selected_outcome])) +
         theme_classic() +
         theme(plot.title = element_text(face = "bold",
                                         size = 18),
               plot.subtitle = element_text(size = 13),
               axis.text.x = element_text(size = 13), 
               axis.text.y = element_text(size = 13),
               axis.title.x = element_text(size = 16, 
                                           margin = margin(t = 20)),
               axis.title.y = element_text(size = 16, 
                                           margin = margin(r = 15)))
        })
       
    # Model 2

       formula <- reactive({
         as.formula(paste(input$selected_outcome2,
                          "~ total +", 
                          paste(input$selected_predictors, 
                                collapse =" + ")))
         
         # This code creates a pre-determined formula based on a selected
         # outcome and predictors. It separates the predictors with a + sign,
         # and then as.formula() activates the expression so it can be used in the
         # next model.
         
          }) 
       
       model_2 <- reactive({
         stan_glm(data = full_data,
                  formula = formula(),
                  
                  # This is the formula I just created.
                  
                  refresh = 0,
                  family = "gaussian")})
         
         output$model2 <- render_gt(
           model_2() %>%
             tbl_regression(intercept = TRUE) %>%
             as_gt() %>%
             tab_header(title = paste("Regression of", 
                                      names(outcome_table)
                                      [outcome_table == input$selected_outcome2]),
                        subtitle = paste("Predicted changes in", 
                                         names(outcome_table)
                                         [outcome_table == input$selected_outcome2],
                                         "as various predictors increase")))
       
  }
  
  shinyApp(ui, server)
  