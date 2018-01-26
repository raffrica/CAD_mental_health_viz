library(shiny)
library(tidyverse)
library(shinydashboard)
library(leaflet)
library(rgdal)


# Reading in Data
getwd()
canada <- readOGR("data","gpr_000a11a_e")
subset <- c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", "Nova Scotia", "Ontario", "Prince Edward Island", "Quebec", "Saskatchewan")
canada <- subset(canada, PRENAME %in% subset)

# Reading in Non-Mapping Data
mental_health_reduced <- read_csv("data/mh_clean.csv")
## FUTURE Buttons
age_input  <- c("Total", "15 to 24", "25 to 44", "45 to 64", "65 +") 
provinces_input <-c("British Columbia", "Alberta", "Saskatchewan", "Manitoba", "Ontario", 
                     "Quebec", "New Brunswick", "Prince Edward Island", "Nova Scotia", "Newfoundland and Labrador")
sex_input <- mental_health_reduced$SEX %>% unique()
unit_input <- mental_health_reduced$UNIT %>% unique() %>% .[2]
stress_level_clean <- c("Low Stress", "High Stress") %>% .[2]
perceived_need_clean <- c("Needs not Met", "Needs Met") %>% .[2]
perceived_mental_health_clean <- c("Poor Mental Health", "Good Mental Health") %>% .[1]
perceptions_input <- c(stress_level_clean, perceived_need_clean, perceived_mental_health_clean)
substance_disorders_clean <- c("Any Substance", "Alcohol", "Cannabis", "Other")
general_disorders_clean <- c("Any Disorder", "Depression", "Bipolar", "Anxiety", "Suicidal", "Psychosis", "PTSD")
disorders_input <- list(general_disorders_clean, substance_disorders_clean)


ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = "Canadian Mental Health: The 2012 Canadian Community Health Survey Visualized", titleWidth = 750),
  
  dashboardSidebar(
    selectInput("perceptionsInput", "Canadian Perceptions",
                choices = perceptions_input,
                selected = perceptions_input[1]),
    
    selectInput("ageInput", "Age",
                choices = age_input,
                selected = age_input[1]), 
    
    radioButtons("sexInput", "Sex",
                choices = sex_input,
                selected = sex_input[1], 
                inline = TRUE)
    ),
  
  dashboardBody(
    fluidRow(
      box(leafletOutput("mymap"), width = 12), width = NULL),
    
    fluidRow(box(title = textOutput("title_figures"), width = 12, solidHeader = TRUE, background = "light-blue"), width = NULL),
    fluidRow(
      column(width = 8,
       tabBox(
         title = "Disorders",
         # The id lets us use input$tabset1 on the server to find the current tab
         id = "tabset1", height = "250px",
         tabPanel("General Mental Health Disorders", plotOutput("general_disorder_plot", height = "250px")),
         tabPanel("Substance Use Disorders", plotOutput("substance_disorder_plot", height = "250px")),
         width = NULL)),
      column(width = 4,
         valueBoxOutput("suicideBox",  width = NULL),
         valueBoxOutput("accessBox",  width = NULL)) #solidHeader = TRUE, width = NULL))
      )
    )
  
  
)

server <- function(input, output){

  
  suicide_count <- reactive({
    # Filter Dataframe based on reactive inputs
    suicide_base <- mental_health_reduced %>% 
      filter(HRPROF == "Suicidal") %>% 
      filter(GEO == province_clicked$prov_clicked_now) %>% 
      filter(UNIT == unit_input) %>% 
      filter(AGE == input$ageInput) %>% 
      filter(SEX == input$sexInput)
    
    suicide_base %>% filter(UNIT == unit_input) %>% .$Value %>% scales::percent()
    
  })
  
  accessed_care_count <- reactive({
    # Filter Dataframe based on reactive inputs
    access_base <- mental_health_reduced %>% 
      filter(HRPROF == "Accessed care (past year)") %>% 
      filter(GEO == province_clicked$prov_clicked_now) %>% 
      filter(UNIT == unit_input) %>% 
      filter(AGE == input$ageInput) %>% 
      filter(SEX == input$sexInput)
    
    access_base %>% filter(UNIT == unit_input) %>% .$Value %>% scales::percent()
    
  })
  
  # Filter Dataframe based on reactive inputs - and for just general disorders
  data_gen <- reactive({
    mental_health_reduced %>% 
      filter(HRPROF %in% disorders_input[[1]]) %>% 
      filter(GEO == province_clicked$prov_clicked_now) %>% 
      filter(UNIT == unit_input) %>% 
      filter(AGE == input$ageInput) %>% 
      filter(SEX == input$sexInput)
    
    })
  
  # Filter Dataframe based on reactive inputs - and for just substance use disorders
  data_subs <- reactive({
    mental_health_reduced %>% 
      filter(HRPROF %in% disorders_input[[2]]) %>% 
      filter(GEO == province_clicked$prov_clicked_now) %>% 
      filter(UNIT == unit_input) %>% 
      filter(AGE == input$ageInput) %>% 
      filter(SEX == input$sexInput)
    
  })
  
  # Output of Plot for General Mental Health Disorders
  output$general_disorder_plot <- renderPlot({
    p_disorders <- ggplot(data_gen()) +
      geom_bar(aes(x = HRPROF, y = Value), stat = "identity", 
               fill = "#3C8DBC",
               colour = "#3C8DBC") +
      labs(y = unit_input, 
           title =paste("Province:", province_clicked$prov_clicked_now, "\n", 
                 "Age:", input$ageInput, "\n", 
                 "Sex:", input$sexInput), 
           x = "Mental Health Disorders") +
      scale_fill_discrete(drop = FALSE) +
      scale_x_discrete(drop = FALSE) +
      theme_minimal()
    
    
    p_disorders + scale_y_continuous(labels = scales::percent_format())
    
    })
  
  # Output of Plot for Substance Use Disorders
  output$substance_disorder_plot <- renderPlot({
    p_disorders <- ggplot(data_subs()) +
      geom_bar(aes(x = HRPROF, y = Value), stat = "identity", 
               fill = "#3C8DBC",
               colour = "#3C8DBC") +
      labs(y = unit_input, 
           title = paste("Province:", province_clicked$prov_clicked_now, "\n", 
                         "Age:", input$ageInput, "\n", 
                         "Sex:", input$sexInput), 
           x = "Mental Health Disorders") +
      scale_fill_discrete(drop = FALSE) +
      scale_x_discrete(drop = FALSE) +
      theme_minimal()
    
    
    p_disorders + scale_y_continuous(labels = scales::percent_format())
    
  })
  
  # Output for Value Box containing the Rate of Suicidality
  output$suicideBox <- renderValueBox({
    valueBox(
      paste(suicide_count()), "Suicidal", icon = icon("list"), color = "light-blue"
    )

  })
  
  
  # Output for Value Box containing value of those who accessed care
  output$accessBox <- renderValueBox({
    valueBox(
      paste(accessed_care_count()), "Accessed Care (past year)", icon = icon("list"), color = "light-blue"
    )
    
  })
  
  data_cloropleth <- reactive({
    mh_data <- mental_health_reduced %>% 
      filter(HRPROF == input$perceptionsInput) %>% 
      filter(UNIT == unit_input) %>% 
      filter(AGE == input$ageInput) %>% 
      filter(SEX == input$sexInput) 
    
    merge(canada, mh_data, by.x = "PRENAME", by.y = "GEO")
    
    
    })
  
  output$mymap <- renderLeaflet({
    data_map <- data_cloropleth()
    
    leaflet() %>% addTiles() %>% 
      addPolygons(data = data_map, 
                  color = "#444444", 
                  weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~colorQuantile("YlOrRd", Value)(Value),
                  highlightOptions = highlightOptions(color = "white", 
                                                      weight = 2,
                                                      bringToFront = TRUE),
                  layerId = ~PRENAME) %>% 
      addLegend(position = "bottomright",pal = colorQuantile("YlOrRd", data_map$Value), value = data_map$Value) %>% 
      addLabelOnlyMarkers(
        lng= -100,
        lat= 65,
        label = paste(input$perceptionsInput),
        labelOptions = labelOptions(noHide = TRUE, textOnly = FALSE, 
                                    textsize = '20px', opacity = 0.8,
                                    direction = "up")
        )
    
    
    })
  
  
  output$title_figures <- renderText({
    paste("Province:", province_clicked$prov_clicked_now)
    
    })
  

  province_clicked <- reactiveValues(prov_clicked_now = "British Columbia")
  
  observeEvent(input$mymap_shape_click, {
    province_clicked$prov_clicked_now <- input$mymap_shape_click$id
    
  })
  
  
  
  
  
  }

shinyApp(ui,server)

