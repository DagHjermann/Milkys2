
current_year <- 2021

source("functions/function_HSDtest.R")
library(shiny)
library(ggplot2)
library(dplyr)

# Data and station metadata
# dat <- readRDS("../Data/109_adjusted_data_2021-09-15.rds")
# dat_stations <- readxl::read_excel("../Input_data/Kartbase_edit.xlsx")
dat <- readRDS("Data/109_adjusted_data_2022-09-23.rds")
dat_stations <- read.csv("Input_data/Lookup_tables/Lookup_stationorder.csv")

c <- 0.1

dat2 <- dat %>%
  filter((VALUE_WW + c) > 0) %>%
  left_join(dat_stations %>% select(STATION_CODE, Order, Station_name, Region), by = "STATION_CODE") %>%
  mutate(
    Matrix = paste0(LATIN_NAME, ", ", TISSUE_NAME),
    log_Conc = log10(VALUE_WW + c),
    Station = paste(STATION_CODE, substr(STATION_NAME, 1, 15)),
    LOQ = ifelse(is.na(FLAG1), "Over LOQ", "Under LOQ")
  ) %>%
  filter(!is.na(log_Conc))

matrices <- dat2 %>%
  filter(MYEAR == current_year) %>%
  count(Matrix) %>%
  arrange(desc(n)) %>%
  pull(Matrix)

years <- sort(unique(dat2$MYEAR))

group_seq <- c("Metals and metalloids", "Chlorobiphenyls", 
               "Polycyclic aromatic hydrocarbons (PAHs)", 
               "Organobromines", 
               "Dichloro-diphenyl-trichloroethane (DDTs)", "Organochlorines (general)",
               "Pesticides", 
               "Chlorinated paraffins", "Organofluorines", "Phenols/chlorophenols", 
               "Organo-metallic compounds", 
               "Chlorinated flame retardants",
               "Biological effects: molecular/biochemical/cellular/assays", 
               "Organic esters", "Isotopes",  
               "Cyclodienes", "Dioxins", "Biomarkers", "Phthalates", 
               "Phosphorus flame retardant (PFR)", "Major inorganic constituents", 
               "Fat and dry weight", "Hexachlorocyclohexanes", 
               "Siloxans", 
               "Others", ""
)

# param_meta <- read.csv2("../Input_data/Lookup for big excel - param.csv") %>%
# param_meta <- read.csv2("Input_data/Lookup for big excel - param.csv") %>%
#   select(Parameter.Code, Substance.Group) %>%
#   mutate(Substance.Group = factor(Substance.Group, levels = group_seq))
param_meta <- read.csv("Input_data/Lookup_tables/Lookup table - substance groups.csv") %>%
  # rename(Parameter.Code = PARAM) %>%
  select(PARAM, Substance.Group)# %>%
  mutate(Substance.Group = factor(Substance.Group, levels = group_seq))

# test
# groups <- unique(param_meta$Substance.Group)
# setdiff(groups, group_seq)
# setdiff(group_seq, groups)

parameters <- dat2 %>%
  filter(MYEAR == current_year) %>%
  distinct(PARAM) %>%
  left_join(param_meta, by = "PARAM") %>%
  arrange(Substance.Group, PARAM) %>%
  pull(PARAM)


