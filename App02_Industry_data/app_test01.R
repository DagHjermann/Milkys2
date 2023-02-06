#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

setwd("/home/jovyan/shared/DHJ/Milkys2/App02_Industry_data")

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

# Lookup file for station names  
lookup_stations <- read.csv("../Input_data/Lookup_tables/Lookup_stationorder.csv") %>%
  mutate(Station = paste(STATION_CODE, Station_name)) %>%
  select(STATION_CODE, Station_name, Station, Region)

lookup_stations <- readRDS("data_chem_industry_ranfjord_elkem_ind_2022.rds") %>%
  rename(Station_name = STATION_NAME) %>%
  distinct(STATION_CODE, Station_name) %>% 
  mutate(Station = paste(STATION_CODE, Station_name))

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
dat_all_prep3 <- readRDS("data_chem_industry_ranfjord_elkem_ind_2022.rds") %>%
  mutate(Basis = "WW",
         Station = paste(STATION_CODE, STATION_NAME))
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
  left_join(lookup_stations %>% select(STATION_CODE, Station_name), by = "STATION_CODE")

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
params <- unique(dat_all_prep3$PARAM) %>% sort()
stations <- unique(dat_all_prep3$Station) %>% sort()
tissues <- unique(dat_all_prep3$TISSUE_NAME) %>% sort()
tissues <- c("(automatic)", tissues)
basises <- unique(dat_all_prep3$Basis) %>% sort()

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


setwd("/home/jovyan/shared/DHJ/Milkys2")

if (FALSE){
  
  data_trend <- get_gam_data(dat_all_prep3, "MYEAR", "VALUE")
  
  param <- "PB"
  param <- "HG"
  st <- "St. 1"
  st <- "I965"
  
  table(dat_all_prep3$STATION_CODE) 

  data_sel <- dat_all_prep3 %>%
    filter(PARAM %in% param & STATION_CODE %in% st) %>%
    rename(x = MYEAR) %>%
    mutate(
      y = log(VALUE),
      LOQ = case_when(
        is.na(FLAG1) ~ as.numeric(NA),
        FLAG1 == "<" ~ y)
    )
  
  data_sel_medians <- get_median_data(data_sel)
  
  data_sel_trend <- get_gam_data(data_sel_medians) %>%
    rename(ymin = y_lo, ymax = y_hi)
  
  chg <- get_change_from_gamdata(data_sel_trend, "x", "y")
  
  get_trendstring(chg)
  
  # debugonce(get_trendstring_comb)
  # debugonce(get_change_from_gamdata)
  get_trendstring_comb(data_sel_trend)
  
  eqs <- get_eqs(param, "Mytilus edulis", "WW")
  
  titlestring <- paste0(data_sel$PARAM[1], " in ", data_sel$LATIN_NAME[1], " at ", data_sel$STATION_CODE[1])

  # debugonce(plot_timeseries_trend)
  plot_timeseries_trend(data_medians = data_sel_medians, 
                        data_raw = data_sel, 
                        data_trend = data_sel_trend, 
                        titlestring = titlestring,
                        eqs = TRUE,
                        value_eqs = eqs,
                        trendtext = get_trendstring(chg))
  
  

}

