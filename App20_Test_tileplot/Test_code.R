
setwd("App20_Test_tileplot/")

library(shiny)

dat_groups <- read.csv("../Input_data/Lookup_tables/Lookup table - substance groups.csv")  

current_year <- 2021

library(ggiraph)
library(ggplot2)
library(lubridate)
library(flextable)
library(readxl)
library(purrr)
library(glue)
library(scico)            # colour palettes incl. "Vik" (https://github.com/thomasp85/scico)
library(cowplot)

library(dplyr)    # need to load dplyr AFTER ggiraph fro some reason, otherwise error "%>% is not found" 

# library(safejoin) # https://github.com/moodymudskipper/safejoin

source("../431_Report_parameter_functions.R")
source("../002_Utility_functions.R")

#
# Stations - for correct ordering
#
lookup_stations <- read.csv("../Input_data/Lookup_tables/Lookup_stationorder.csv")

# Text strings for the plot y axis  
eider_blood <- "19N Kongsfjorden (eider duck blood)"
eider_egg <- "19N Kongsfjorden (eider duck egg)"


param <- "HG"
serieslastuntil <- 2021
basis_medians <- "WW"
species3 <- "Mytilus edulis"

header_param <- subset(dat_groups, PARAM %in% param, select = Parameter.Name)[1,1]

species3 <- strsplit(species3, split = ",")[[1]]

debugonce(get_data_medians)

dat_temporary <- list(
  # Cod
  get_data_medians(param = param,     # In Shiny: input$param
                   species = "Gadus morhua",
                   tissue = ifelse(param %in% "HG", "Muskel", "Lever"),
                   basis = basis_medians, 
                   include_year = serieslastuntil,
                   folder_110 = "../Data",
                   filename_110 = "110_mediandata_updated_2022-09-23.rds",
                   filename_lookup_substancegroups = "../Input_data/Lookup_tables/Lookup table - substance groups.csv",
                   filename_lookup_stations= "../Input_data/Lookup_tables/Lookup_stationorder.csv",
                   filename_lookup_eqs = "../Input_data/Lookup_tables/Lookup_EQS_limits.csv",
                   filename_lookup_proref = "../Input_data/Lookup_tables/Lookup_proref.csv"))





setwd("..")


