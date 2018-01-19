library(shiny)
library(tidyverse)
library(shinydashboard)
library(leaflet)
library(rgdal)


# Reading in Data
getwd()
canada <- readOGR("src","gpr_000a11a_e")
subset <- c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", "Nova Scotia", "Ontario", "Prince Edward Island", "Quebec", "Saskatchewan")
canada <- subset(canada, PRENAME %in% subset)

# Reading in Non-Mapping Data
mental_health_reduced <- read_csv("data/mental_health.csv")
## FUTURE Buttons
age_input  <- c("Total", "15 to 24", "25 to 44", "45 to 64", "65 +") 
provinces_input <-c("British Columbia", "Alberta", "Saskatchewan", "Manitoba", "Ontario", 
                     "Quebec", "New Brunswick", "Prince Edward Island", "Nova Scotia", "Newfoundland and Labrador")
sex_input <- mental_health_reduced$SEX %>% unique()
unit_input <- mental_health_reduced$UNIT %>% unique()
stress_level_clean <- c("Low Stress", "High Stress")
perceived_need_clean <- c("Needs not Met", "Needs Met")
perceived_mental_health_clean <- c("Poor Mental Health", "Good Mental Health")
perceptions_input <- c(stress_level_clean, perceived_need_clean, perceived_mental_health_clean)
substance_disorders_clean <- c("Any Substance", "Alcohol", "Cannabis", "Other")
general_disorders_clean <- c("Any Disorder", "Depression", "Bipolar", "Anxiety", "Suicidal", "Psychosis", "PTSD")
disorders_input <- list(general_disorders_clean, substance_disorders_clean)



ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = "Mental Health Info"),
  
  dashboardSidebar(
    selectInput("perceptionsInput", "Canadian Perceptions",
                choices = perceptions_input,
                selected = perceptions_input[1]),
    
    selectInput("ageInput", "Age",
                choices = age_input,
                selected = age_input[1]), 
    
    selectInput("provincesInput", "Province",
                choices = provinces_input,
                selected = provinces_input[1]), 
    
    selectInput("unitInput", "Unit of Measurement",
                choices = unit_input,
                selected = unit_input[1])
    ),
  
  dashboardBody(
    fluidRow(
      box(leaflet() %>% addTiles() %>% 
      addPolygons(data = canada, weight = 2), width = 12)),
    fluidRow(
      column(width = 8,
       tabset(
         title = "Disorders",
         # The id lets us use input$tabset1 on the server to find the current tab
         id = "tabset1", height = "250px",
         tabPanel("General Mental Health ", plotOutput("disorder_plot", height = "250px")),
         tabPanel("Substance Abuse", plotOutput("disorder_plot", height = "250px")), 
         width = NULL)),
      column(width = 4,
        box(title = "HI", width = NULL),
        box(title = "HIHIH", width = NULL))
      )
    )
  
  
)

server <- function(input, output){
  
  output$disorder_plot <- renderPlot({
    p_disorders <- ggplot(mental_health_reduced %>% 
             filter(HRPROF %in% disorders_input[[1]]) %>% 
             filter(GEO == input$provincesInput) %>% 
             filter(UNIT == input$unitInput) %>% 
             filter(AGE == input$ageInput)) +
      geom_bar(aes(x = HRPROF, y = Value), stat = "identity", 
               fill = "light blue",
               colour = "light blue") +
      labs(y = "Percent", 
           title = paste("Province:", input$provincesInput, "\n", "Age:", input$ageInput), 
           x = "Mental Health Disorders") +
      scale_fill_discrete(drop = FALSE) +
      scale_x_discrete(drop = FALSE) +
      theme_minimal()
    
    
    if (input$unitInput == "Number") p_disorders
    else p_disorders + scale_y_continuous(labels = scales::percent_format())
    
    
    })
  
  
  
  
  }

shinyApp(ui,server)
