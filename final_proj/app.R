
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

ui <- fluidPage(
  
  navbarPage("Sex Education and Women and Girls' Health",

             # Panel 1
             
             tabPanel("SDG 5.6.2.: Sexual Education",
                      h1("How does each country rank in sex education?"),
                      p("Based on the UN Sustainable Development Goals", style = "font-size:20px;"),
                      br(),
                      
                      # Main Panel
                      mainPanel(
                        selectInput(inputId = "selected_country",
                                    label = "Choose a country or region",
                                    choices = sex_ed$country,
                                    selected = "Africa"),
                        
                        plotOutput("country_plot"),
                        br(),
                        p("Within target 5.6., I chose to focus specifically on indicator 5.6.2: the extent to which countries have laws and regulations that guarantee women aged 15-49", strong("access to sexual and reproductive health care, information and education."), "Each country gets a score for this indicator, as well as for other specific measures related to this goal."), 
                        p("The scores are reported as", a(href = "https://unstats.un.org/sdgs/files/meetings/webex-6sep2018/6.%20UNFPA%205.6.2%20Presentation.pdf", "percentage scores"), "between 0 and 100, reflecting", strong("the extent to which such laws and regulations exist."), "Beyond the total score, I have to chosen to include 1) to what extent Comprehensive Sexual Education (CSE) is present in the law, 2) to what extent CSE is present in school curriculums, and 3) access to family planning and contraception.")),
                      
                      # Side Panel
                      sidebarPanel(
                        h4("SDG #5: Achieve gender equality and empower all women and girls"),
                        h5("Target 5.6.2: Having laws and regulations that guarantee full and equal access to women and men aged 15 years and older to sexual and reproductive health care, information and education")),
                      br(),
                      h2("About the Data"),
                      p("The United Nations'", a(href="https://sdgs.un.org/goals", "Sustainable Develoment Goals"), "(SDGs) were adopted in 2015 as a global blueprint for peace and prosperity. As a unit, they recognize the strategic importance of ending poverty through improving health and education, as well as reducing inequalities. They also reaffirm the UN's dedication to tackling climate change and preserving the environment."),
                      p("SDG #5 is to", strong("achieve gender equality and empower all women and girls."), "Improving women's and girls' health is a key step towards that goal. Many targets within this goal aim to improve health; I am interested at understanding how one target in particular – target 5.6: Ensure universal access to sexual and reproductive health and reproductive rights – can ameliorate women and girls' health around the world.")
             ),
             
             # Panel 2
             
             tabPanel("Data",
                      h2("How Well Does Sexual Education Predict Health Outcomes Compared with Other Indicators?"),
                      p("In the following tab, I will be making models to determine how well sexual education scores can", strong("predict a selection of health outcomes.")), 
                      p("For this purpose, I collected data on health outcomes per country, as well as on various predictors that may have a", strong("stronger predictive value on the outcomes.")),
                      p("You can take a look at all the data here!"),
                      br(),
                      h4("Health Outcomes"),
                      dataTableOutput('health'),
                      p(em("Sources: UN Stats, World Health Organization, Institute for Health Metrics and Evaluation, World Development Indicators, United Nations Development Programme", style = "font-size:10px;")),
                      h4("Predictors"),
                      dataTableOutput('predictors'),
                      p(em("Sources: UN Stats, World Development Indicators, Center for Systemic Peace, UN Population Division, United Nations Development Programme", style = "font-size:10px;")),
                      p("* The Democracy and Autocracy Scores are calculated on a scale from 0 to 10. The Polity Score ranges from -10 to 10.", style = "font-size:10px;")
                      
                 
             ),
             
             # Panel 3
             
             tabPanel("Models",
                      mainPanel(
                         h3("Model 1: Simple Regression"),
                         p("This first model regresses a chosen health outcome on all four SDG scores."),
                         p("Choose an outcome below to visualize how it is impacted by sexual education."),
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
                         br(),
                         h3("Model 2: Predictors"),
                         selectInput(inputId = "selected_outcome2",
                                     label = "Choose a health outcome:",
                                     choices = c("Teen Pregnancy Rate" = "pregnancy_rate", 
                                                 "Contraception Prevalence" = "contracept_prev", 
                                                 "Youth HIV Rate" = "youth_hiv", 
                                                 "Maternal Mortality" = "matern_mort", 
                                                 "Total Life Expectancy" = "total_life_exp", 
                                                 "Female Life Expectancy" = "female_life_exp"),
                                     selected = "matern_mort"),
                         p("For this next model, we will hold constant the predictors you choose to estimate the causal effect of sexual education laws by controlling for other confounding factors. Choose any variable that you think may be a confounding variable; i.e., that may be influencing health outcomes and biasing our evaluation of the causal effect of sexual education laws."),
                         selectInput(inputId = "selected_predictors",
                                     label = "Choose predictors that you think affect the efficacy of sex education laws:",
                                     choices = c("Urbanization Rate" = "percent_urban_pop",
                                                 "Median Age" = "med_age",
                                                 "HDI" = "human_devel_index",
                                                 "Inequality" = "inequality_coef",
                                                 "GDP per Capita" = "gdp_capita",
                                                 "School Enrollment Rate (girls)" = "school_enroll_girls",
                                                 "Gender Inequality Index" = "gend_ineq_index",
                                                 "Youth Dependency Ratio" = "youth_depend_ratio",
                                                 "Primary Healthcare Expenditures" = "phc_expend",
                                                 "Total Government Expenditures" = "gov_expend",
                                                 "Expenditures on Reproductive Health" = "reprod_health_expend",
                                                 "Expenditures on Family Planning" = "family_plann_expend",
                                                 "State Fragility Index" = "state_frag_index",
                                                 "Democracy Score" = "democ",
                                                 "Autocracy Score" = "autoc",
                                                 "Polity Score" = "polity",
                                                 "World Region" = "region",
                                                 "Total Population" = "pop_count",
                                                 "Youth Literacy Rate" = "youth_literacy_rate",
                                                 "Poverty Rate" = "poverty_rate",
                                                 "Income Group" = "income_group"),
                                     multiple = TRUE,
                                     selected = c("school_enroll_girls", "inequality_coef")),
                         gt_output("model2"),
                         ),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        gt_output("model1"),
                        br(),
                        p("The coefficients in this regression table represent the", strong("estimated effect of each SDG score on the chosen health outcome,"), "if all other predictors in the model are held constant at zero. Therefore, to estimate the effect of the total score for SDG 5.6.2 on your chosen outcome, just look at the value under Beta, in the total row.", em("Each unit increase of the total score"), "is estimated to increase or decrease the value of the health indicator by that amount."),
                        p("The confidence interval to the right of the coefficient depicts", strong("incertainty"), "around this value. As you can see, there are wide confidence intervals in this model, suggesting that the results are not very precise. This is why, in the next model, we will be controlling for many other predictors, in an attempt to get a", strong("more precise estimate.")),
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
                        p("Looking at this regression table, the Total SDG Score for sexual education seems to be a", strong("pretty poor predictor of health outcomes for women and girls."),"The ", em("95% Confidence Interval"), "is often huge, and the coefficient is", em("relatively insignificant"), "compared to other predictors. Inequality, for example, seems to be a much stronger predictor for many health outcomes."),
                        p("These results suggest that", strong("sexual education alone may not be sufficient for improving health outcomes"), "for women and girls. The predictors with the highest coefficients and lowest uncertainty, such as School Enrollment Rates for Girls or Gender Inequality Index, are those that impact health the most. These results suggest that such issues should be addressed in priority, before sexual education laws can be effective.")

               
             ),
             
             # Panel 4
             
              tabPanel("About",
                mainPanel(
                 h3("This is an About Me! My name is Salomé"),
                 h5("Summary"),
                 p("This project focuses on sexual education around the world, and its relation to health outcomes for women and girls. I use data from the sustainable development goals, which calculate scores for each country based on the extent to which sexual education is available. For this analysis, I was interested in determining how well a country’s sexual education profile could predict health outcomes for women and girls, and found that the relationship, while positive, was not very strong. Other indicators, such as inequality and school enrollment for girls, seem to be better predictors of the outcomes I observed."),
                 p("This is where I will explain the motivation for this project and talk about my data and process."),
                 h5("Data"),
                 p("Here is a link to my final project repo: https://github.com/salomegarnier/Final_Project.git."),
                 p("Most of my data is from UN Stats, WHO, and IHME. Most are from 2017 to 2020. You can find it in the raw data folder."),
                 p("At this point, all I have left I think is adding some text/interpretation. ")),
                br(),
                br(),
                img(src = "IMG_4260.jpg", height = 200)
  )))
  
# Server

  server <- function(input, output, session) {
    
    # Plot 1
    
    output$country_plot <- renderPlot({
      
      sex_ed %>%
        
        pivot_longer(names_to = "sdg", values_to = "value", cols = total:sex_edu_laws) %>%
        filter(country == input$selected_country) %>%
        
        ggplot(aes(x = sdg, y = value)) + 
        geom_col(aes(fill = sdg)) +
        geom_text(aes(x = sdg, 
                      label = value),
                  size = 10,
                  y = 10,
                  color = "#585563") +
        theme_classic() +
        labs(title = paste("Sexual Education in", input$selected_country),
             x = "Sustainable Development Goal Indicator",
             y = "Score (out of 100)") +
        md_theme_classic() +
        theme(axis.text.x = element_markdown(size = 13), 
              axis.text.y = element_markdown(size = 13),
              plot.title = element_markdown(size = 20, 
                                        face = "bold"), 
              axis.title.x = element_markdown(size = 16, margin = margin(t = 20)),
              axis.title.y = element_markdown(size = 16),
              legend.position = "bottom",
              legend.text = element_markdown(size = 12),
              legend.title = element_blank()) +
        scale_x_discrete(breaks = c("curriculum", "family_planning", "sex_edu_laws", "total"), 
                         labels = c("Curriculum", "Family Planning", "Sex Education Laws", "SDG Total Score")) +
        scale_fill_manual(breaks = c("curriculum", "family_planning", "sex_edu_laws", "total"),
                            labels = c("**Curriculum:** extent to <br>which comprehensive <br>sexual education is <br>included in school curricula",
                                       "**Family Planning:** <br>access to family <br>planning resources <br>and contraception",
                                       "**Sex Education Laws:** extent <br>to which comprehensive <br>sexual education is <br>included in country laws",
                                       "**SDG Total Score:** extent to which <br>countries have laws and regulations <br>that guarantee access to sexual and <br>reproductive health care, information <br>and education"),
                          values = c("#C9B1BD", "#7EBDC2", "#F3DFA2", "#8C7DA1")) +
        ylim(c(0, 100))
    })
    
    
    # Tables 
    
    output$predictors <- renderDataTable(predictors %>% mutate(across(is.numeric, ~ round(., 2))), 
                                         options = list(pageLength = 5, scrollX = T),
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
    
    output$health <- renderDataTable(health_outcomes %>% mutate(across(is.numeric, ~ round(., 2))), 
                                     options = list(pageLength = 5, scrollX = T), 
                                     class = "compact hover order-column",
                                     colnames = c("Country", "Teen Pregnancy Rate", 
                                                  "Contraception Prevalence", "Youth HIV Rate", 
                                                  "Maternal Mortality", "Total Life Expectancy", 
                                                  "Female Life Expectancy"))
    
    
  # Model 1
    
    model_1 <- reactive({
                 stan_glm(data = full_data,
                          formula = get(input$selected_outcome) ~ total + family_planning + curriculum + sex_edu_laws,
                          refresh = 0,
                          family = "gaussian")})
    
    # Render GT Table
    
    output$model1 <- render_gt(
      model_1() %>%
        tbl_regression() %>%
        as_gt() %>%
        tab_header(title = paste("Regression of", names(outcome_table)[outcome_table == input$selected_outcome]),
                   subtitle = paste("The Effect of Sexual Education on", names(outcome_table)[outcome_table == input$selected_outcome])))
    
    
    
    
    
    
    # 
    # full_data <- reactive({
    # 
    # model_1 <- stan_glm(data = full_data(),
    #          formula = get(input$selected_outcome) ~ total + family_planning + curriculum + sex_edu_laws,
    #          refresh = 0,
    #          family = "gaussian")})
    # 
    # # Render GT Table
    # 
    # output$model1 <- render_gt(
    #   model_1 %>%
    #   tbl_regression() %>%
    #   as_gt() %>%
    #   tab_header(title = paste("Regression of", names(outcome_table)[outcome_table == input$selected_outcome]),
    #              subtitle = paste("The Effect of Sexual Education on", names(outcome_table)[outcome_table == input$selected_outcome])))
    
        # Predict
    
    predict_1 <- reactive({predict(model_1(), newdata = new_obs, se.fit = TRUE)})
    
        # Plot
    
     output$model_1_plot <- renderPlot({
      tibble(total = seq(0, 100, .1),
             prediction = predict_1()$fit,
             se = predict_1()$se.fit) %>%
        ggplot(aes(total, prediction)) +
        geom_line(size = 1.5, color = "#38618C", alpha = 2) +
        geom_errorbar(aes(ymin = prediction - 1.96*se, ymax = prediction + 1.96*se),
                      alpha = 0.2, color = "#ACECF7") +
         labs(title = paste("Predictions of", names(outcome_table)[outcome_table == input$selected_outcome], "based on SDG 5.6.2."),
              subtitle = paste("The Estimated Effect of SDG 5.6.2. total score on", names(outcome_table)[outcome_table == input$selected_outcome]),
              x = "SDG 5.6.2. total score",
              y = paste("Predicted", names(outcome_table)[outcome_table == input$selected_outcome])) +
         theme_classic() +
         theme(plot.title = element_text(face = "bold", size = 18),
               plot.subtitle = element_text(size = 13),
               axis.text.x = element_text(size = 13), 
               axis.text.y = element_text(size = 13),
               axis.title.x = element_text(size = 16, margin = margin(t = 20)),
               axis.title.y = element_text(size = 16, margin = margin(r = 15)))
        })
       
    # Model 2

       formula <- reactive({
         as.formula(paste(input$selected_outcome2, "~ total +", paste(input$selected_predictors, 
                                                                      collapse =" + ")))
          }) 
       
       model_2 <- reactive({
         stan_glm(data = full_data,
                  formula = formula(),
                  refresh = 0,
                  family = "gaussian")})
         
         output$model2 <- render_gt(
           model_2() %>%
             tbl_regression() %>%
             as_gt() %>%
             tab_header(title = paste("Regression of", names(outcome_table)[outcome_table == input$selected_outcome2]),
                        subtitle = paste("The Effect of Various Predictors on", names(outcome_table)[outcome_table == input$selected_outcome2])))
       
  }
  
  shinyApp(ui, server)
  