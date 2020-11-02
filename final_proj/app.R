
library(shiny) # you may need to install.packages() this
library(tidyverse)

library(shiny)
library(fec16)

# this is just a normal object

sex_ed <- read_csv("sex_education_clean.csv")
full_data <- read_csv("fulldataset.csv")

######################################################################################
######################################################################################
# 

# User interface

ui <- fluidPage(
  
  navbarPage("SDGs: Sex Education in the World",

             # Panel 1
             
             tabPanel("Main",
                      sidebarPanel("How does each country rank in sex education?"), 
                      mainPanel(
                        selectInput(inputId = "selected_country",
                                    label = "Choose a country or region",
                                    choices = sex_ed$country),
                        
                        plotOutput("country_plot")
                        
                        ),
             ),
             
             # Panel 2
             
             tabPanel("Sex Education and Health Indicators",
                      plotOutput("sexed_indic")
                      
             # Panel 3
             
             ),
              tabPanel("About",
                 h3("This is an About Me! My name is Salomé"),
                 p("Here is a link to my final project repo: https://github.com/salomegarnier/Final_Project.git."),
                 p("The data I have so far is from UN Stats, WHO, and IHME. You can find it in the raw data folder."),
                 p("Next steps include: cleaning the two other data sets that I want to work with and joining them together to make plots that include data from each source."),
                 p("One challenge I am facing is the fact that some countries in my original dataset are missing.")
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
        geom_col(fill = "#E26D5C") +
        geom_text(aes(x = sdg, 
                      label = value),
                  size = 10,
                  y = 10,
                  color = "#585563") +
        theme_classic() +
        labs(title = paste("Sex Education Laws in ", input$selected_country),
             x = "Sustainable Development Goal",
             y = "Score (out of 100)") +
        theme(axis.text.x = element_text(size = 13), 
              axis.text.y = element_text(size = 13),
              plot.title = element_text(size = 20, 
                                        face = "bold"), 
              axis.title.x = element_text(size = 16, margin = margin(t = 20)),
              axis.title.y = element_text(size = 16)) +
        scale_x_discrete(breaks = c("curriculum_laws", "family_planning", "sex_edu", "total"), 
                         labels = c("Curriculum Laws", "Family Planning", "Sex Education", "SDG Total Score")) +
        ylim(c(0, 100))
    })
    
    # Plot 2
    
    output$sexed_indic <- renderPlot({
      full_data %>%
        drop_na() %>%
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
    
  }
  
  shinyApp(ui, server)
  