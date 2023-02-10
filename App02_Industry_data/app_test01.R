#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Set 
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

dataset1 <- readRDS("data_chem_industry_ranfjord_elkem_ind_2022.rds")
# dataset1b <- readRDS("../Data/109_adjusted_data_2022-09-23.rds") %>%
#   filter(STATION_CODE %in% "15B",
#          PARAM %in% c("PYR1OH","PYR1O")) %>%
#   mutate(STATION_NAME = "Skågskjera, Farsund",
#          BASIS = "WW") %>%
#   select(-VALUE) %>%
#   rename(VALUE = VALUE_WW)

dataset2 <- readRDS("data_chem_industry_kristiansand_glencore_ind_2022.rds")

dat_all_prep3 <- bind_rows(dataset1, dataset1b, dataset2) %>%
# dat_all_prep3 <- bind_rows(dataset1, dataset1b, dataset2) %>%
  mutate(Basis = "WW",
         Station = case_when(
           is.na(STATION_CODE) ~ STATION_NAME,
           STATION_CODE %in% "" ~ STATION_NAME,
           TRUE ~ paste(STATION_CODE, STATION_NAME))
  ) %>%
  filter(!is.na(VALUE))

# df_trend <- readRDS(paste0(folder_output, "/126_df_trend_2021.rds"))

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
  
  param <- "PB"
  param <- "HG"
  param <- "NI"
  param <- "PYR1O"
  param <- "PYR1OH"
  stcode <- "St. 1"
  stcode <- "I965"
  stcode <- "15B"
  # stcode <- "I969"
  #st <- "St. 1 Lumber"
  #st <- "Hanneviksbukta"

  # table(dat_all_prep3$STATION_CODE) 
  # table(dat_all_prep3$Station) %>% names()

  data_sel <- dat_all_prep3 %>%
    filter(PARAM %in% param & STATION_CODE %in% stcode) %>%
    #filter(Station %in% st) %>% # xtabs(~PARAM, .)
    filter(PARAM %in% param) %>%
    rename(x = MYEAR) %>%
    mutate(
      y = log(VALUE),
      LOQ = case_when(
        is.na(FLAG1) ~ as.numeric(NA),
        FLAG1 == "<" ~ y)
    ) %>% filter(x >= 2001)
  
  # debugonce(get_median_data)
  data_sel_medians <- get_median_data(data_sel)
  
  data_sel_trend <- get_gam_data(data_sel_medians) %>%
    rename(ymin = y_lo, ymax = y_hi)
  
  chg <- get_change_from_gamdata(data_sel_trend, "x", "y")
  
  get_trendstring(chg)
  
  # debugonce(get_trendstring_comb)
  # debugonce(get_change_from_gamdata)
  get_trendstring_comb(data_sel_trend)
  
  eqs <- get_eqs(param, "Mytilus edulis", "WW", eqsdata = lookup_eqs)
  proref <- get_proref(param, "Mytilus edulis", basis = "WW", prorefdata = lookup_proref)
  
  titlestring <- paste0(data_sel$PARAM[1], " in ", data_sel$LATIN_NAME[1], " at ", data_sel$Station[1])

  # debugonce(plot_timeseries_trend)
  plot_timeseries_trend(data_medians = data_sel_medians, 
                        data_raw = data_sel, 
                        data_trend = data_sel_trend, 
                        titlestring = titlestring,
                        eqs = TRUE, proref = "1,2", value_proref = proref,
                        value_eqs = eqs,
                        trendtext = get_trendstring(chg))
  
  

}

if (FALSE){
  
  dat_all_prep3 <- readRDS(paste0(folder_input, "/125_dat_all_prep3.rds"))

  dat_pyr10oh_lista <- readRDS("Data/109_adjusted_data_2022-09-23.rds") %>%
    filter(STATION_CODE %in% "15B")
  
  
}

