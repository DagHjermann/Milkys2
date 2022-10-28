#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(purrr)
library(ggplot2)
library(lubridate)
library(stringr)
# library(AICcmodavg)   # AICc()
library(svglite)
# font_import(pattern = "Wingdings.ttf")    # if not already installed on your PC

# Functions  
source("../125_Calculate_trends_leftadjusted_functions.R")
source("../402_Plot_time_series_functions.R")

# Lookup file for station names  
lookup_stations <- read.csv("../Input_data/Lookup_tables/Lookup_stationorder.csv") %>%
  mutate(Station = paste(STATION_CODE, Station_name)) %>%
  select(STATION_CODE, Station_name, Station, Region)

# Lookup files for EQS and Proref   
lookup_eqs <- read.csv("../Input_data/Lookup_tables/Lookup_EQS_limits.csv") %>%
  filter(Basis %in% c("WW", "WWa")) %>%
  select(-Long_name, -Kommentar) %>%
  rename(EQS = Limit)
lookup_proref <- read.csv("../Input_data/Lookup_tables/Lookup_proref.csv") %>%
  filter(Basis %in% c("WW", "WWa")) %>%
  select(PARAM, LATIN_NAME, TISSUE_NAME, Basis, Proref) 

# Lookup file for full parameter names
lookup_paramnames <- readxl::read_excel("../Input_data/Lookup table - parameter names for plots.xlsx")

# Lookup file for species names
lookup_speciesnames <- read.csv("../Input_data/Lookup_tables/Lookup_speciesnames.csv")

# Folders from where to fetch trend analysis results and fits  
folder_results <- "../Data/125_results_2021_07"
folder_input <- paste0(folder_results, "_input")
folder_output <- paste0(folder_results, "_output")

# Data
dat_series_trend <- readRDS(paste0(folder_input, "/125_dat_series_trend.rds")) %>%
  left_join(lookup_stations %>% select(STATION_CODE, Station), by = "STATION_CODE")
dat_all_prep3 <- readRDS(paste0(folder_input, "/125_dat_all_prep3.rds"))
df_trend <- readRDS(paste0(folder_output, "/126_df_trend_2021.rds"))

# Add 'Param_name' and 'Tissue_name' to data    
dat_all_prep3 <- dat_all_prep3 %>%
  left_join(lookup_paramnames, by = "PARAM") %>%
  mutate(
    Param_name = ifelse(is.na(Param_name), PARAM, Param_name),  # use PARAM if Param_name is lacking
    Tissue_name = case_when(
      TISSUE_NAME %in% "Lever" ~ "Liver",
      TISSUE_NAME %in% "Muskel" ~ "Muscle",
      TISSUE_NAME %in% "Galle" ~ "Bile",
      TRUE ~ TISSUE_NAME)
  )

# Add 'Species_name' to data    
dat_all_prep3 <- dat_all_prep3 %>%
  left_join(lookup_speciesnames, by = "LATIN_NAME") %>%
  mutate(Species_name = ifelse(is.na(Species_name), LATIN_NAME, Species_name))

# Add station names + Region 
dat_all_prep3 <- dat_all_prep3 %>%
  left_join(lookup_stations %>% select(STATION_CODE, Station_name, Region), by = "STATION_CODE")

# Add EQS and Proref to data    
dat_all_prep3 <- bind_rows(
  dat_all_prep3 %>%
    filter(PARAM != "CB118") %>%
    left_join(lookup_eqs %>% filter(PARAM != "CB118") %>% select(-LATIN_NAME, -Basis), by = c("PARAM")),
  dat_all_prep3 %>%
    filter(PARAM == "CB118") %>%
    left_join(lookup_eqs %>% filter(PARAM == "CB118"), by = c("PARAM", "Basis", "LATIN_NAME"))
) %>%
  left_join(lookup_proref, by = c("PARAM", "LATIN_NAME", "TISSUE_NAME", "Basis"))

# For the menus
params <- unique(dat_series_trend$PARAM) %>% sort()
stations <- unique(dat_series_trend$Station) %>% sort()
tissues <- unique(dat_series_trend$TISSUE_NAME) %>% sort()
tissues <- c("(automatic)", tissues)
basises <- unique(dat_series_trend$Basis) %>% sort()

# Folder for saving plots
folder <- "../Figures_402/Til 2021-rapporten/"

# Save metadata for saved plots
savedplots_filename <- paste0(folder, "_saved_plots.csv")  
file_exists <- file.exists(savedplots_filename)
if (!file_exists){
  zz <- file(savedplots_filename, "w")  # open an output file connection
  cat("Filename, Time_GMT, PARAM, STATION_CODE, TISSUE_NAME, Basis, y_scale, ymax_perc, xmin_rel, xmax_rel, eqs, proref\n", file = zz)
  close(zz)
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Milkys time series"),

    # Sidebar with menus 
    sidebarLayout(
        sidebarPanel(
          shiny::selectInput(inputId = "param", label = "Parameter", choices = params, selected = "HG"),
          shiny::selectInput(inputId = "station", label = "Station code", choices = stations, selected = "98B1"),
          shiny::selectInput(inputId = "tissue", label = "Tissue", choices = tissues, selected = "(automatic)"),  
          shiny::selectInput(inputId = "basis", label = "Basis", choices = basises, selected = "WW"),
          shiny::selectInput(inputId = "y_scale", label = "Y-scale", choices = c("ordinary", "log numbers", "log scale"), 
                             selected = "ordinary"),
          shiny::checkboxInput("eqs", "Include EQS line", value = TRUE),
          shiny::textInput("proref", "Proref lines, separated by comma (e.g. 1,2,5)", value = "1"),
          shiny::checkboxInput("medians", "Show medians", value = TRUE),
          shiny::checkboxInput("allsamples", "Show single measurements", value = FALSE),
          shiny::sliderInput("ymax_perc", "Max y (%)", value = 100, min = 2, max = 200, step = 2),
          shiny::sliderInput("xmin_rel", "Change x min.", value = 0, min = -10, max = 40, step = 0.5),
          shiny::sliderInput("xmax_rel", "Change x max.", value = 0, min = -40, max = 10, step = 0.5)
          
        ),

        # Show the plot, with save button underneath  
        mainPanel(
           plotOutput("timeseriesplot", width = "500px", height = "500px"),
           shiny::actionButton("save", "Save plot"),
           br(), br(),
           shiny::textInput("filename_add", "Text to add to filename (e.g., '_ver02')", value = "")

        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  #
  # Get ggplot object   
  #
  
  tsplot_r <- reactive({
    stationcode <- subset(lookup_stations, Station %in% input$station)$STATION_CODE
    latinname <- dat_all_prep3 %>% 
      filter(STATION_CODE == stationcode) %>% 
      pull(LATIN_NAME) %>% unique() %>% head(1)
    if (latinname %in% "Mytilus edulis"){
      quantiles <- c(0,1)
    } else {
      quantiles <- c(0.25, 0.75)
    }
    if (input$tissue == "(automatic)"){
      tsplot <- plot_timeseries(param = input$param, stationcode = stationcode, basis = input$basis, 
                                y_scale = input$y_scale,
                                ymax_perc = input$ymax_perc,
                                xmin_rel = input$xmin_rel,
                                xmax_rel = input$xmax_rel,
                                eqs = input$eqs,
                                proref = input$proref,
                                folder = folder_results, 
                                data = dat_all_prep3, 
                                data_series = dat_series_trend, data_trend = df_trend, 
                                quantiles = quantiles,
                                medians = input$medians,
                                allsamples = input$allsamples)
    } else {
      tsplot <- plot_timeseries(param = input$param, stationcode = stationcode, basis = input$basis, 
                                y_scale = input$y_scale,
                                ymax_perc = input$ymax_perc,
                                xmin_rel = input$xmin_rel,
                                xmax_rel = input$xmax_rel,
                                eqs = input$eqs,
                                proref = input$proref,
                                folder = folder_results, 
                                data = dat_all_prep3, 
                                data_series = dat_series_trend, data_trend = df_trend, 
                                quantiles = quantiles,
                                medians = input$medians,
                                allsamples = input$allsamples)
      
    }
    tsplot
  })
  
  #
  # Show on screen
  #
  output$timeseriesplot <- renderPlot({
    
    tsplot_r()
    
  }, width = 600, height = 450)
  
  #
  # Save to file
  #
  
  observeEvent(input$save, {
    stationcode <- subset(lookup_stations, Station %in% input$station)$STATION_CODE
    if (input$tissue == "(automatic)"){
      fn_base <- paste(input$param, stationcode, sep = "_")
    } else {
      fn_base <- paste(input$param, stationcode, input$tissue, sep = "_")
    }
    # Save plot
    fn_base <- paste0(fn_base, input$filename_add)
    save_plot(tsplot_r(), paste0("../Figures_402/Til 2021-rapporten/", fn_base, ".png"))
    
    # Write metadata for saved plot
    fileconn <- file(savedplots_filename, "at")
    cat(paste(paste0(fn_base, ".png"), as.character(Sys.time()),
              input$param, stationcode, input$tissue, input$basis, input$y_scale, 
              input$ymax_perc, input$xmin_rel, input$xmax_rel, 
              as.character(input$eqs), sQuote(input$proref), sep = ","), "\n",
        file = fileconn)
    close(fileconn)
    
  })
  
    
}

# Run the application 
shinyApp(ui = ui, server = server)
