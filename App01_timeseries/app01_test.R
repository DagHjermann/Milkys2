#
# Code for loading data for testing ----
#

# Run/source the entire thing before testing functions



load_data <- TRUE 

if (load_data){
  
  # From the top of App 01, so we need setwd() in the start AND at the end  
  setwd("App01_timeseries/")
  
  
  # The rest is copied from the top of App 01 (28.10.2022)
  
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
  
  # ALSO RUN THIS!  
  setwd("..")
  
  
}


if (FALSE){
  
  # Testing ----
  
  input <- list()
  input$param <- "HG"
  input$param <- "VDSI"
  stationcode <- "30B"
  stationcode <- "36G"
  latinname = "Gadus morhua"
  latinname = "Nucella lapillus"
  
  input$tissue <- "(automatic)"  
  input$basis <- "WW"  
  input$y_scale  <- "ordinary"
  input$eqs <- TRUE
  input$proref <- "1"
  input$medians <- TRUE
  input$allsamples <- FALSE
  input$ymax_perc <- 100
  input$xmin_rel <- 0
  input$xmax_rel <- 0
  
  if (latinname %in% "Mytilus edulis"){
    quantiles <- c(0,1)
  } else {
    quantiles <- c(0.25, 0.75)
  }
  
  setwd("/home/jovyan/shared/DHJ/Milkys2/App01_timeseries")
  
  # debugonce(plot_timeseries)
  # debugonce(plot_timeseries_seriesno)
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
  
  dat_all_prep3 %>%
    filter(STATION_CODE %in% stationcode, PARAM %in% input$param) %>%
    group_by(MYEAR) %>%
    summarize(Value = median(VALUE_WW)) %>%
    mutate(Value2 = exp(Value)) %>%
    View()
  
  setwd("/home/jovyan/shared/DHJ/Milkys2")
  
  rm(list = c("input", stationcode, latinname, quantiles))
  
  
}


if (FALSE){
  
  # Remove all objects made above ----
  
  # ls() %>% dput()
  
  app_objects <- c("basises", "dat_all_prep3", "dat_series_trend", "df_trend", 
    "extract_difference_data", "extract_modelfit_data", "extract_raw_data", 
    "folder", "folder_input", "folder_output", "folder_results", 
    "get_seriesno", "get_splines_results", "get_splines_results_seriesno", 
    "get_splines_results_seriesno_s", "get_trendstring", "get_unit_text", 
    "load_data", "lookup_eqs", "lookup_paramnames", "lookup_proref", 
    "lookup_speciesnames", "lookup_stations", "params", "plot_timeseries", 
    "plot_timeseries_seriesno", "save_plot", "stations", "tissues", 
    "tsplot_param", "tsplot_seriesno")
  
  rm(list = c(app_objects, "app_objects"))
  
  
}
