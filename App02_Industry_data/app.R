#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# startup: packages ----

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
# source("../402_Plot_time_series_functions.R")
source("app_functions.R")

# startup: lookup files ----  

# Lookup file for station names  
lookup_stations1 <- readRDS("data_chem_industry_ranfjord_elkem_ind_2022_OLD2.rds") %>%
  rename(Station_name = STATION_NAME) %>%
  distinct(STATION_CODE, Station_name) %>%
  filter(!STATION_CODE %in% c("I964/I964b", "I965", "I969"))

lookup_stations2 <- tibble::tribble(
  ~STATION_CODE, ~Station_name,
  "I964/I964b", "Toraneskaia",
  "I965", "Moholmen",
  "I969", "Bjørnbærviken"
)



lookup_stations <- bind_rows(
  lookup_stations1, lookup_stations2) %>%
  mutate(Station = paste(STATION_CODE, Station_name))

# Lookup files for EQS and Proref   
lookup_eqs <- read.csv("../Input_data/Lookup_tables/Lookup_EQS_limits.csv") %>%
  filter(Basis %in% c("WW", "WWa")) %>%
  select(-Long_name, -Kommentar) %>%
  rename(EQS = Limit)
lookup_proref <- read.csv("../Input_data/Lookup_tables/Lookup_proref.csv") %>%
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
# startup: data ----  

# NOTE: 
# These data sets have been created by Dag using scripts
#   994_Industry_data_2022_Ranfjorden_Elkem.Rmd
#   994_Industry_data_2022_Glencore.Rmd
# in folder/project "Milkys"
# 

dataset_all <- readRDS("data_chem_industry_2023_complete.rds") %>%
  bind_rows(readRDS("data_chem_industry_hoyangsfjord_2007-2024.rds")) %>%
  bind_rows(readRDS("data_chem_industry_ranfjord_2024.rds")) %>% 
  bind_rows(readRDS("data_chem_industry_krsand-elkem_2024.rds")) %>% 
  bind_rows(readRDS("data_chem_industry_krsand-glencore_2024.rds"))

# dat_all_prep3 <- bind_rows(dataset1, dataset2) %>%
dat_all_prep1 <- dataset_all %>%
  mutate(
    Basis = case_when(
      BASIS %in% "W" ~ "WW",
      BASIS %in% "D" ~ "DW"),
    VALUE = case_when(
      VALUE == 0 & PARAM %in% "Sum 16 EPA-PAH ekskl. LOQ" ~ 0.05,
      TRUE ~ VALUE),
    Station = case_when(
      is.na(STATION_CODE) ~ STATION_NAME,
      STATION_CODE %in% "" ~ STATION_NAME,
      TRUE ~ paste(STATION_CODE, STATION_NAME))
  ) %>%
  filter(!is.na(VALUE))

# Add 'Param_name' and 'Tissue_name' to data    
dat_all_prep2a <- dat_all_prep1 %>%
  left_join(lookup_paramnames, by = "PARAM") %>%
  mutate(
    Param_name = ifelse(is.na(Param_name), PARAM, Param_name),  # use PARAM if Param_name is lacking
    Tissue_name = case_when(
      TISSUE_NAME %in% "Lever" ~ "Liver",
      TISSUE_NAME %in% "Muskel" ~ "Muscle",
      TISSUE_NAME %in% "Galle" ~ "Bile",
      grepl("Whole soft body", TISSUE_NAME) ~ "Whole soft body",
      TRUE ~ TISSUE_NAME)
  )

# Add 'Species_name' to data    
dat_all_prep2b <- dat_all_prep2a %>%
  left_join(lookup_speciesnames, by = "LATIN_NAME") %>%
  mutate(Species_name = ifelse(is.na(Species_name), LATIN_NAME, Species_name))

# Add EQS and Proref to data    
dat_all_prep3 <- bind_rows(
  dat_all_prep2b %>%
    filter(PARAM != "CB118") %>%
    left_join(lookup_eqs %>% filter(PARAM != "CB118") %>% select(-LATIN_NAME), 
              by = c("PARAM", "Basis"),  relationship = "many-to-one"),
  dat_all_prep2b %>%
    filter(PARAM == "CB118") %>%
    left_join(lookup_eqs %>% filter(PARAM == "CB118"), 
              by = c("PARAM", "Basis", "LATIN_NAME"),
              relationship = "many-to-one")
) %>% 
  mutate(
    UNIT = case_when(
      UNIT %in% "NG_P_G" ~ "UG_P_KG",
      UNIT %in% "UG_P_KG_WW" ~ "UG_P_KG",
      UNIT %in% "PERCENT" ~ "%",
      UNIT %in% "PG_P_G" ~ "NG_P_KG",
      TRUE ~ UNIT)) %>%
  left_join(lookup_proref, by = c("PARAM", "LATIN_NAME", "TISSUE_NAME", "Basis"))

# Final transformation
sel <- with(dat_all_prep3, UNIT %in% "PG_P_G" & !(PARAM %in% c("Dioksiner", "Dioksiner og dioksinliknende PCB", "Dioksinliknende PCB")))
if (sum(sel) > 0){
  dat_all_prep3$UNIT[sel] <- "UG_P_KG"
  dat_all_prep3$VALUE[sel] <- dat_all_prep3$VALUE[sel]*0.001
}

# For the menus
params <- unique(dat_all_prep3$PARAM) %>% sort()
# projects <- unique(dat_all_prep3$Projects) %>% sort()
stations <- unique(dat_all_prep3$Station) %>% sort()
tissues <- unique(dat_all_prep3$TISSUE_NAME) %>% sort()
tissues <- c("(automatic)", tissues)
basises <- unique(dat_all_prep3$Basis) %>% sort()
years_all <- unique(dat_all_prep3$MYEAR) %>% sort()
names(years_all) <- years_all

# browser()

# Check if all observations in a series has the same units
series_conflicting_units <- dat_all_prep3 %>% 
  group_by(PARAM, Station, Basis) %>% 
  summarize(
    n_unit = length(unique(UNIT)),
    units = paste(unique(UNIT), collapse = ", ")
  ) %>% 
  filter(
    n_unit > 1
  )

# If there are conflicting units, delete 
if (nrow(series_conflicting_units) > 0){
  # Add 'n_unit' and 'units' to data for series with conflicting units, and keep only series with conflicting units  
  check_unit <- dat_all_prep3 %>% 
    left_join(series_conflicting_units, by = join_by(PARAM, Station, Basis), relationship = "many-to-one") %>% 
    filter(!is.na(n_unit))
  warning("The following series contains more than one unit and will be deleted. Check 'check_unit'")
  print(series_conflicting_units)
  # Delete series with conflicting units
  dat_all_prep3 <- dat_all_prep3 %>% 
    left_join(series_conflicting_units, by = join_by(PARAM, Station, Basis), relationship = "many-to-one") %>% 
    filter(is.na(n_unit))
}


# Folder for saving plots
folder <- "../Figures_402/Til 2024-rapporten/"

# Create folder if it doesn't exist
if (!dir.exists(folder)){
  dir.create(folder)}

# Save metadata for saved plots
savedplots_filename <- paste0(folder, "_saved_plots.csv")  
file_exists <- file.exists(savedplots_filename)
if (!file_exists){
  zz <- file(savedplots_filename, "w")  # open an output file connection
  cat("Filename, Time_GMT, PARAM, STATION_CODE, TISSUE_NAME, Basis, y_scale, ymax_perc, xmin_rel, xmax_rel, eqs, proref\n", file = zz)
  close(zz)
}




# UI ----

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  shinyjs::useShinyjs(),
  
  # Application title
  titlePanel("Industry data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      shiny::selectInput(inputId = "param", label = "Parameter", choices = params, selected = "Dioksiner"),
      shiny::selectInput(inputId = "station", label = "Station", choices = stations, selected = "Glencore kai"),
      shiny::selectInput(inputId = "tissue", label = "Tissue", choices = tissues, selected = "(automatic)"),  
      shiny::selectInput(inputId = "basis", label = "Basis", choices = basises, selected = "WW"),
      # Make button for showing hidden settings
      # Code from Albert Rapp, https://gist.github.com/AlbertRapp/ed866ecaf6c245519cd6d6fca55b56f1  
      actionLink(
        'show_hidden_settings', 
        'Show/hide selected years', 
        icon = icon('caret-right')
      ),
      # The secret settings
      shinyjs::hidden(
        div(
          id = 'hidden_stuff',
          checkboxGroupInput("selected_years",
                             "Selected years:",
                             years_all,
                             selected = years_all)
        )
      ),
      br(), br(),
      shiny::selectInput(inputId = "y_scale", label = "Y-scale", choices = c("ordinary", "log numbers", "log scale"), 
                         selected = "ordinary"),
      shiny::checkboxInput("eqs", "Include EQS line", value = TRUE),
      shiny::textInput("proref", "Proref lines, separated by comma (e.g. 1,2,5)", value = "1"),
      shiny::checkboxInput("medians", "Show medians", value = TRUE),
      shiny::checkboxInput("allsamples", "Show single measurements", value = FALSE),
      shiny::sliderInput("ymin_perc", "Min y (%), fine-tune with arrow keys", value = 100, min = 0.1, max = 250, step = 0.1),
      shiny::sliderInput("ymax_perc", "Max y (%), fine-tune with arrow keys", value = 100, min = 0.1, max = 250, step = 0.1),
      shiny::sliderInput("xmin_rel", "Change x min.", value = 0, min = -10, max = 40, step = 0.5),
      shiny::sliderInput("xmax_rel", "Change x max.", value = 0, min = -40, max = 10, step = 0.5),
      shiny::sliderInput("x_step", "Distance between x labels (0 = auto)", value = 0, min = 0, max = 10, step = 1)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("timeseries_plot"),
      shiny::actionButton("save", "Save plot"),
      br(), br(),
      shiny::textInput("plot_folder", "Folder name (inside 'App02_Industry_data')", value = "Figures_2022"),
      shiny::textInput("filename_add", "Text to add to filename (e.g., '_ver02')", value = "")
    )
  )
)

# SERVER ----

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #
  # Get data for plot ----
  #
  
  data_sel_filter <- reactive({
    #if (input$param == "CO" & input$station == "Dvergsøya (referansestasjon)")
    #if (input$param == "BAP" & input$station == "B2 Alterneset")
    #if (input$param == "Sum 16 EPA-PAH ekskl. LOQ" & input$station == "B2 Alterneset")
    # browser()
    dat_all_prep3 %>%
      filter(PARAM %in% input$param & Station %in% input$station & Basis %in% input$basis &
               MYEAR %in% as.numeric(input$selected_years))
  })
  
  data_sel_transformation <- reactive({
    data_sel_filter <- data_sel_filter()
    if (min(data_sel_filter$VALUE) > 0){
      result <- "log"
    } else {
      result <- "log_plus_1"
    }
    result
  })
  
  data_sel <- reactive({
    #if (input$param == "CO" & input$station == "Dvergsøya (referansestasjon)")
    #if (input$param == "BAP" & input$station == "B2 Alterneset")
    #if (input$param == "Sum 16 EPA-PAH ekskl. LOQ" & input$station == "B2 Alterneset")
    # browser()
    data_sel_filter <- data_sel_filter()
    result <- data_sel_filter %>%
      rename(x = MYEAR) %>%
      mutate(
        LOQ = case_when(
          is.na(FLAG1) ~ as.numeric(NA),
          FLAG1 == "<" ~ VALUE)
      )
    transformation <- data_sel_transformation()
    if (transformation == "log"){
      result <- result %>%
        mutate(y = log(VALUE))
    } else {
      result <- result %>%
        mutate(y = log(VALUE + 1))
    }
    result
  })
  
  latin_name <- reactive({
    data_sel()$LATIN_NAME[1]
  })
  
  quantiles <- reactive({
    if (latin_name() %in% "Mytilus edulis"){
      quantiles <- c(0,1)
    } else {
      quantiles <- c(0.25, 0.75)
    }
    quantiles
  })

  data_sel_medians <- reactive({
    get_median_data(data_sel(), quantiles = quantiles())
    })
  
  data_sel_trend <- reactive({
    get_gam_data(data_sel_medians()) %>%
      rename(ymin = y_lo, ymax = y_hi)
  })

  # For testing only
  # - must also add plot
  # output$data_rows <- renderPrint({
  #   nrow(data_sel_trend())
  #   })
  
  #
  # Get other values for plot ----
  #
  
  titlestring <- reactive({
    paste0(input$param, " in ", latin_name(), " at ", input$station)
  })
  
  trendstring <- reactive({
    # browser()
    get_trendstring_comb(data_sel_trend())
  })

  eqs <- reactive({
    # browser()
    get_eqs(input$param, latin_name(), input$basis, eqsdata = lookup_eqs)
  })
  
  proref <- reactive({
    get_proref(input$param, latin_name(), basis = input$basis, prorefdata = lookup_proref)
  })
  
  #
  # Create plot ----
  #
  output$timeseries_plot <- renderPlot({
  
    # browser()
    
    data_sel <- data_sel() 
    
    # if (tail(data_sel$PARAM, 1) == "Dioksiner")
    #   browser()
    
    unit_print <- get_unit_text(
      tail(data_sel$UNIT, 1), 
      tail(data_sel$Basis, 1), 
      tail(data_sel$PARAM, 1))
    
    plot_timeseries_trend(data_medians = data_sel_medians(),
                          data_raw = data_sel, 
                          data_trend = data_sel_trend(),
                          y_scale = input$y_scale, 
                          ymin_perc = input$ymin_perc, 
                          ymax_perc = input$ymax_perc, 
                          xmin_rel = input$xmin_rel, xmax_rel = input$xmax_rel,
                          x_step =  input$x_step,
                          titlestring = titlestring(),
                          y_label = unit_print,
                          trendtext = trendstring(),
                          quantiles = quantiles(),
                          eqs = input$eqs,
                          proref = input$proref,
                          value_eqs = eqs(), 
                          value_proref = proref(),
                          transformation = data_sel_transformation()
                          )
  })
  
  #
  # Create plot for saving ----
  #
  timeseries_plot_for_file <- reactive({
    
    data_sel_medians <- data_sel_medians() 
    
    unit_print <- get_unit_text(
      tail(data_sel_medians$UNIT, 1), 
      tail(data_sel_medians$Basis, 1), 
      tail(data_sel_medians$PARAM, 1))

    plot_timeseries_trend(data_medians = data_sel_medians,
                          data_raw = data_sel(), 
                          data_trend = data_sel_trend(),
                          y_scale = input$y_scale, 
                          ymin_perc = input$ymin_perc, 
                          ymax_perc = input$ymax_perc, 
                          xmin_rel = input$xmin_rel, xmax_rel = input$xmax_rel,
                          x_step =  input$x_step,
                          titlestring = titlestring(),
                          y_label = unit_print,
                          trendtext = trendstring(),
                          quantiles = quantiles(),
                          eqs = input$eqs,
                          proref = input$proref,
                          value_eqs = eqs(), value_proref = proref())
  })
  
  
  
  #
  # Save to file ----
  #
  
  observeEvent(input$save, {
    stationcode <- subset(lookup_stations, Station %in% input$station)$STATION_CODE
    if (input$tissue == "(automatic)"){
      fn_base <- paste(input$param, stationcode, sep = "_")
    } else {
      fn_base <- paste(input$param, stationcode, input$tissue, sep = "_")
    }
    # Weed out slashes in the station code....
    fn_base <- sub("/", "", fn_base, fixed = TRUE)
    # Save plot
    fn_base <- paste0(fn_base, input$filename_add)
    fn <- paste0(input$plot_folder, "/", fn_base, ".png")
    # browser()
    ggplot <- timeseries_plot_for_file() +
      theme(axis.text = element_text(size = 11),     # set size of numbers along axes
            axis.title = element_text(size = 12),    # set size of axis labels
            plot.title = element_text(size = 13))    # set size of plot title
    width = 5.6; height = 4.4
    #svglite(fn, height = height*1.2, width = width*1.2)                              ELU:testing with svglite (not png)
    png(fn, height = height*1.2, width = width*1.2, units = "in", res = 400)    # create an empty plot file 
    print(ggplot)                                                                         # ...plot on it...
    dev.off()      

  })
  
  #
  # Show secret settings on click ----
  #
  observe({
    shinyjs::toggle(
      id = 'hidden_stuff',
      anim = TRUE
    )
    updateActionLink(
      inputId = 'show_hidden_settings',
      icon = if (input$show_hidden_settings %% 2 == 0) {
        icon('caret-right')
      } else {
        icon('caret-down')
      }
    )
  }) |> bindEvent(input$show_hidden_settings)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
