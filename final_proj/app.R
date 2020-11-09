
library(shiny)
library(tidyverse)
library(shiny)
library(fec16)
library(ggtext)
library(mdthemes)

sex_ed <- read_csv("sex_education_clean.csv")
full_data <- read_csv("fulldataset.csv")
predictors <- read_csv("predictors_clean.csv")
health_outcomes <- read_csv("health_outcomes_clean.csv")

######################################################################################
######################################################################################
# 

# User interface

ui <- fluidPage(
  
  navbarPage("Sex Education and Women and Girls' Health",

             # Panel 1
             
             tabPanel("SDG 5.6.2.: Sexual Education",
                      h1("How does each country rank in sex education?"), 
                      
                      # Main Panel
                      mainPanel(
                        selectInput(inputId = "selected_country",
                                    label = "Choose a country or region",
                                    choices = sex_ed$country,
                                    selected = "Africa"),
                        
                        plotOutput("country_plot"),
                        br(),
                        p("Within target 5.6., I chose to focus specifically on indicator 5.6.2: the extent to which countries have laws and regulations that guarantee women aged 15-49 years access to sexual and reproductive health care, information and education. Each country gets a score for this indicator, as well as for other specific measures related to this goal."), 
                        p("The scores are reported as", a(href = "https://unstats.un.org/sdgs/files/meetings/webex-6sep2018/6.%20UNFPA%205.6.2%20Presentation.pdf", "percentage scores"), "between 0 and 100, reflecting the extent to which such laws and regulations exist. Beyond the total score, I have to chosen to include 1) to what extent Comprehensive Sexual Education (CSE) is present in the law, 2) to what extent CSE is present in school curriculums, and 3) access to family planning and contraception.")),
                      
                      # Side Panel
                      sidebarPanel(
                        h4("SDG #5: Achieve gender equality and empower all women and girls"),
                        h5("Target 5.6.2: Extent to which countries have laws and regulations that guarantee full and equal access to women and men aged 15 years and older to sexual and reproductive health care, information and education")),
                      br(),
                      h2("About the Data"),
                      p("The United Nations'", a(href="https://sdgs.un.org/goals", "Sustainable Develoment Goals"), "(SDGs) were adopted in 2015 as a global blueprint for peace and prosperity. As a unit, they recognize the strategic importance of ending poverty through improving health and education, as well as reducing inequalities. They also reaffirm the UN's dedication to tackling climate change and preserving the environment."),
                      p("SDG #5 is to achieve gender equality and empower all women and girls. Improving women's and girls' health is a key step towards that goal. Many targets within this goal aim to improve health; I am interested at understanding how one target in particular – target 5.6: Ensure universal access to sexual and reproductive health and reproductive rights – can ameliorate women and girls' health around the world."),
                      p("For this purpose, I am interested in the relationship between countries' scores on target 5.6. and specific health outcomes related to women and girls. I chose to look only at health, but, obviously, these targets serve a broader purpose of female empowerment that go beyond health outcomes. These other aspects, however, are beyond the scope of this project.")
                      
             ),
             
             # Panel 2
             
             tabPanel("Model",
                      h2("Are Health Outcomes Impacted by Sex Education Laws?"),
                      p("I now have a larger dataset that I will be able to use to make my model. It includes many predictors about each country, that I can use as controls, as well as several health outcomes I am interested in."),
                      br(),
                      h4("Health Outcomes"),
                      dataTableOutput('health'),
                      h4("Predictors"),
                      dataTableOutput('predictors'),
                      plotOutput("sexed_indic"),
                 
             ),
             
             # Panel 3
             
              tabPanel("About",
                 h3("This is an About Me! My name is Salomé"),
                 p("Here is a link to my final project repo: https://github.com/salomegarnier/Final_Project.git."),
                 p("Most of my data is from UN Stats, WHO, and IHME. Most are from 2017 to 2020. You can find it in the raw data folder."),
                 p("Next steps include: making the model!! I think I have all the data I need now.")
  )))
  
# Server

  server <- function(input, output, session) {
    
    # Plot 1
    
    output$country_plot <- renderPlot({
      
      sex_ed %>%
        
        pivot_longer(names_to = "sdg", values_to = "value", cols = total:sex_edu) %>%
        filter(country == input$selected_country) %>%
        
        # this plot is just like normal!
        ggplot(aes(x = sdg, y = value)) + 
        geom_col(aes(fill = sdg)) +
        geom_text(aes(x = sdg, 
                      label = value),
                  size = 10,
                  y = 10,
                  color = "#585563") +
        theme_classic() +
        labs(title = paste("Sex Education Laws in", input$selected_country),
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
        scale_x_discrete(breaks = c("curriculum_laws", "family_planning", "sex_edu", "total"), 
                         labels = c("Curriculum", "Family Planning", "Sex Education Laws", "SDG Total Score")) +
        scale_fill_manual(breaks = c("curriculum_laws", "family_planning", "sex_edu", "total"),
                            labels = c("**Curriculum:** extent to <br>which comprehensive <br>sexual education exists <br>in school curricula",
                                       "**Family Planning:** <br>access to family <br>planning resources <br>and contraception",
                                       "**Sex Education Laws:** extent <br>to which comprehensive <br>sexual education exists in <br>country laws",
                                       "**SDG Total Score:** extent to which <br>countries have laws and regulations <br>that guarantee access to sexual and <br>reproductive health care, information <br>and education"),
                          values = c("#C9B1BD", "#7EBDC2", "#F3DFA2", "#8C7DA1")) +
        ylim(c(0, 100))
    })
    
    # Plot 2
    
    output$sexed_indic <- renderPlot({
      full_data %>%
        drop_na(total, hiv_rate) %>%
        ggplot(aes(total, hiv_rate)) +
        geom_point(color = "#C884A6", size = 3) +
        geom_smooth(method = lm, se = FALSE, color = "#716969") +
        theme_linedraw() +
        labs(title = "Relationship between sex education laws and HIV incidence",
             subtitle = "As of now, this is not very telling",
             x = "Sex Education Laws",
             y = "HIV Incidence Rate") +
        theme(plot.title = element_text(face = "bold", size = 28)) +
        scale_y_continuous(breaks = c(4e+05, 5e+05, 6e+05, 7e+05), labels = c("400 000", "500 000", "600 000", "700 000"))
      
    })
    
    # Tables 
    
    output$predictors <- renderDataTable(predictors, options = list(pageLength = 5))
    
    output$health <- renderDataTable(health_outcomes, options = list(pageLength = 5))
    
  }
  
  shinyApp(ui, server)
  