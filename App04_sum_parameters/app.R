#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# For testing code parts:
# setwd("App04_sum_parameters")
# After testing:
# setwd(".."); getwd()

library(shiny)

library(dplyr)
library(ggplot2)
library(glue)

source("../002_Utility_functions.R")
source("../101_Combine_with_legacy_data_functions.R")

data_all2 <- readRDS("../Data/109_adjusted_data_2022-09-23.rds")  

params <- names(sum_parameters)

data_all2_by_samplepar_all <- data_all2 %>%
  filter(MYEAR >= 2010) %>% # View()  
  mutate(
    VALUE_WW_wo_loq = case_when(
      is.na(FLAG1) ~ VALUE_WW,
      !is.na(FLAG1) ~ 0)
  )



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Median lower and upper bound for sum parameters"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput("param", "Sum parameter", choices = params),
          uiOutput("selectInput_station")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  data_parameter <- reactive({
    data_all2_by_sample <- data_all2_by_samplepar_all %>%
      filter(PARAM %in% sum_parameters[[input$param]]) %>%
      group_by(STATION_CODE, LATIN_NAME, TISSUE_NAME, MYEAR, SAMPLE_NO2, UNIT) %>%
      summarize(
        VALUE_lb = sum(VALUE_WW_wo_loq),
        VALUE_ub = sum(VALUE_WW),
        n_param = n(), .groups = "drop")
    
    data_all2_by_station <- data_all2_by_sample %>%
      group_by(STATION_CODE, LATIN_NAME, TISSUE_NAME, MYEAR, UNIT) %>%
      summarize(
        N_over_zero = sum(VALUE_lb > 0),
        Min_over_zero = min(VALUE_lb[VALUE_lb > 0]),
        Max_over_zero = max(VALUE_lb[VALUE_lb > 0]),
        VALUE_lb = median(VALUE_lb),
        VALUE_ub = median(VALUE_ub), .groups = "drop") %>%
      mutate(
        DDI = case_when(
          N_over_zero > 1 ~ glue("{N_over_zero} [{round(Min_over_zero,2)}-{round(Max_over_zero,2)}]"),
          N_over_zero == 1 ~ glue("{N_over_zero} [{round(Max_over_zero,2)}]"),
          N_over_zero == 0 ~ glue("{N_over_zero} (-)")
        )
      )
    data_all2_by_station
  })
  
  output$selectInput_station <- renderUI({
    stations <- unique(data_parameter()$STATION_CODE)
    selectInput("station", "Station", choices = stations)
  })
  
  data_parameter_station <- reactive({
    data_parameter() %>%
      filter(STATION_CODE %in% input$station)
  })
    
    
  output$distPlot <- renderPlot({
    
    data_plot <- data_parameter_station()
    gg <- ggplot(data_plot, aes(MYEAR, ymin = VALUE_lb, ymax = VALUE_ub)) +
      geom_errorbar(width = 0.2)
    print(gg)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
