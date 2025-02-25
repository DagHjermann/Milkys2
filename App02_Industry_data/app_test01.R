#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Code from the app, before UI and server parts ----
# - ie. runs when the app starts
#
# . set work directory ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

setwd("/home/jovyan/shared/common/DHJ/Milkys2/App02_Industry_data")

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
    UNIT = case_when(
      UNIT %in% "NG_P_G" ~ "UG_P_KG",
      TRUE ~ UNIT),
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
  left_join(lookup_proref, by = c("PARAM", "LATIN_NAME", "TISSUE_NAME", "Basis"))

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




#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# . set work directory back to normal ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

setwd("/home/jovyan/shared/common/DHJ/Milkys2")



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Test area ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


if (F){
  
  table(dataset2$MYEAR)
  table(dat_all_prep3$Station)
  
}



if (FALSE){
  
  # . select PARAM, Station -------------
  
  param <- "PB"
  param <- "HG"
  param <- "NI"
  param <- "PYR1O"
  param <- "PYR1OH"
  param <- "BAP"
  param <-  "Dioksiner og dioksinliknende PCB"
  param <- "Sum 16 EPA-PAH ekskl. LOQ"
  stcode <- "St. 4"
  #stcode <- "I965"
  #stcode <- "15B"
  #stcode <- "15B"
  # stcode <- "I969"
  #st <- "St. 1 Lumber"
  #st <- "Hanneviksbukta"
  st <- "Glencore kai"
  st <- "Dvergsøya (referansestasjon)"
  st <- "B2 Alterneset"
  st <- "St. 1 Lumber"
  st <- "St. 4 Svensholmen"
  
  # Table Station 1-5  
  dat_all_prep3 %>%
    filter(substr(Station,1,2) == "St") %>%
    xtabs(~MYEAR + Station, .)
  
  # table(dat_all_prep3$STATION_CODE) 
  # table(dat_all_prep3$Station) %>% names()
  
  if (FALSE){ 
    # # some tables  
    dat_all_prep3 %>% filter(Station %in% st) %>% xtabs(~PARAM, .)
    dat_all_prep3 %>% filter(PARAM %in% input$param & Station %in% input$station & Basis %in% input$basis) %>% nrow()
    dat_all_prep3 %>% filter(PARAM %in% input$param & Station %in% input$station) %>% nrow()
    dat_all_prep3 %>% filter(PARAM %in% input$param) %>% nrow()
  }
  
  # . select data_sel ------------------------------------
  
  data_sel <- dat_all_prep3 %>%
    filter(PARAM %in% param) %>%
    # filter(STATION_CODE %in% stcode) %>%
    filter(Station %in% st) %>% # xtabs(~PARAM, .)
    rename(x = MYEAR) %>%
    mutate(
      y = log(VALUE),
      LOQ = case_when(
        is.na(FLAG1) ~ as.numeric(NA),
        FLAG1 == "<" ~ y)
    ) %>% filter(x >= 2001)
  
  # debugonce(get_median_data)
  
  # . compute medians --------------------------
  data_sel_medians <- get_median_data(data_sel)
  
  # . test plot 
  ggplot(data_sel_medians, aes(x,y)) + geom_ribbon(aes(ymin = ymin, ymax = ymax), fill = "grey70") + geom_point() 
  
  
  # . get trend data --------------------------------------
  # debugonce(get_gam_data)
  data_sel_trend <- get_gam_data(data_sel_medians) %>%
    rename(ymin = y_lo, ymax = y_hi)
  
  # . test trend data  
  ggplot(data_sel_trend, aes(x,y)) + geom_ribbon(aes(ymin = ymin, ymax = ymax), fill = "grey70") + geom_point() 
  
  # . get trend string ------------------------------------
  # debugonce(get_change_from_gamdata)
  chg <- get_change_from_gamdata(data_sel_trend, "x", "y")
  get_trendstring(chg)
  
  # debugonce(get_trendstring_comb)
  # debugonce(get_change_from_gamdata)
  get_trendstring_comb(data_sel_trend)
  
  # . get EQS and PROREF ---------------------------------------------
  eqs <- get_eqs(param, "Mytilus edulis", "WW", eqsdata = lookup_eqs)
  proref <- get_proref(param, "Mytilus edulis", basis = "WW", prorefdata = lookup_proref)
  
  # . get title string --------------------------------
  titlestring <- paste0(data_sel$PARAM[1], " in ", data_sel$LATIN_NAME[1], " at ", data_sel$Station[1])

  # . make main plot ----------------------------------
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

if (FALSE){
  
  # Test data for metals 2010-2014 
  
  # dataset_all_01 %>%
  # dataset_all_02 %>%
  dataset_all %>%
    filter(STATION_CODE == "St. 4" & PARAM %in% c("AS", "PB", "CD", "CU")) %>%
    ggplot(aes(MYEAR, VALUE, color = FLAG1)) +
    geom_jitter(width = 0.1) +
    facet_wrap(vars(PARAM), scales = "free_y")

}


if (FALSE){
  
  # . data directly from Vannmiljø------  
  
  dataset_extra2_raw <- readxl::read_excel(
    "Vannmiljo St. 4 Svensholmen metals 2010-2014.xlsx", sheet = 1)
  
  # Lookup table for parameter names from Vannmiljø
  # NOTE: specially made for this Vannmiljø file, i.e. has metals only
  lookup_params_vannmiljo <- data.frame(
    Parameter = c("Arsen", "Bly", "Kadmium", "Kobber", "Krom", "Kvikksølv", 
                  "Nikkel", "Sink"),
    PARAM = c("AS", "PB", "CD", "CU", "CR", "HG",
              "NI", "ZN")
  )
  
  table(dataset_extra2_raw$Parameter)
  # Prepare data set from Vannmiljø
  dataset_extra2 <- dataset_extra2_raw %>%
    mutate(
      STATION_CODE = case_when(
        grepl("Svensholmen", Navn) ~ "St. 4"),
      STATION_NAME = case_when(
        grepl("Svensholmen", Navn) ~ "Svensholmen"),
      MYEAR = as.numeric(substr(Prøvedato, 1, 4)),
      Month = as.numeric(substr(Prøvedato, 6, 7)),
      UNIT = case_when(
        Enhet %in% "mg/kg v.v." ~ "MG_P_KG"),
      BASIS = case_when(
        Enhet %in% "mg/kg v.v." ~ "W"),
      TISSUE_NAME = case_when(
        Navn %in% "Mytilus edulis" ~ "Whole soft body"),
      FLAG1 = case_when(
        Operator %in% "<" ~ "<",
        Operator %in% "=" ~ as.character(NA))
    ) %>%
    rename(
      LATIN_NAME = Art,
      VALUE = Verdi) %>%
    filter(
      Month >= 8) %>%
    left_join(
      lookup_params_vannmiljo, by = "Parameter") %>%
    select(-c(VannlokalitetID, Navn, Aktivitet, Oppdragsgiver, Oppdragstaker, Parameter, Medium, Prøvedato, Operator, Enhet, `Sist endret`))
  
  
}

if (FALSE) {
  
  # . data directly from Vannmiljø, tables------  
  
  table(dataset_all$PARAM)
  table(subset(dataset_all, UNIT %in% "MG_P_KG")$PARAM)
  table(dataset_all$UNIT)
  table(dataset_all$BASIS)
  table(dataset_all$LATIN_NAME)
  table(dataset_all$TISSUE_NAME)
  table(dataset_all$STATION_CODE)
  table(dataset_all$STATION_NAME)
  table(dataset_all$MYEAR)
  table(dataset_all$Month)
  
  table(dataset_extra2$Navn)
  table(dataset_extra2$Enhet)
  str(dataset_extra2$Verdi)
  table(addNAdataset_extra2$Operator)
  table(dataset_extra2$Art)
  table(dataset_extra2$Prøvedato)
  str(dataset_extra2$Prøvedato)
  tr(dataset_extra2$Prøvedato)
  table(dataset_extra2$Parameter) %>% names() %>% dput()
  table(substr(dataset_extra2$Prøvedato,6,7))
  
}


