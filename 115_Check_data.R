
library(ggplot2)
library(dplyr)

#
# read data -----------------------------------------------------------
#

# Check last year's data, downloaded from Nivabase
dat_nivabase <- readRDS("Input_data/80_df_2023_notstandard_2024-09-23_20h06m.rds")

# Check all years' data, at different stages
dat_newpart <- readRDS("Data/101_dat_new_2024-09-25.rds")
dat_101 <- readRDS("Data/101_data_updated_2024-09-25.rds")
dat_103 <- readRDS("Data/103_data_updated_2024-09-25.rds")
dat_105 <- readRDS("Data/105_data_with_uncertainty_2024-09-25.rds")
dat_109 <- readRDS("Data/109_adjusted_data_ELU_2024-09-25.rds")

#
# FLAG1 -----------------------------------------------------------
#

# table: FLAG1 (under LOQ) + MYEAR
xtabs(~addNA(FLAG1) + MYEAR, subset(dat_nivabase, MYEAR > 2018))
xtabs(~addNA(FLAG1) + MYEAR, subset(dat_101, MYEAR > 2018))
xtabs(~addNA(FLAG1) + MYEAR, subset(dat_109, MYEAR > 2018))

#
# stations -----------------------------------------------------------
#
# table: STATION_CODE + MYEAR
xtabs(~STATION_CODE + MYEAR, subset(dat_nivabase, MYEAR > 2018))
xtabs(~STATION_CODE + MYEAR, subset(dat_109, MYEAR > 2018))

#
# eider duck -----------------------------------------------------------
#

# table: PARAM + MYEAR for eider duck  
xtabs(~NAME + MYEAR, subset(dat_nivabase, MYEAR > 2018 & STATION_CODE == "19N"))
xtabs(~PARAM + MYEAR, subset(dat_109, MYEAR > 2018 & STATION_CODE == "19N"))

#
# xCCP -----------------------------------------------------------
#

# SCCP and MCCP - check whether values are missing (NA), zero or >zero 
dat_109 %>%
  filter(
    PARAM %in% c("SCCP eksl. LOQ", "SCCP inkl. LOQ", "MCCP eksl. LOQ", "MCCP inkl. LOQ"),
  ) %>%
  mutate(
    VALUE_class = case_when(
      is.na(VALUE_WW) ~ "NA",
      VALUE_WW %in% 0 ~ "0",
      VALUE_WW > 0 ~ ">0")
  ) %>%
  xtabs(~VALUE_class + MYEAR, .)

#
# EROD og ALAD -----------------------------------------------------------
#
# SCCP and MCCP - check whether values are missing (NA), zero or >zero 
dat_nivabase %>%
  filter(
    NAME %in% c("EROD", "ALAD", "PROTV") & !is.na(VALUE) & MYEAR >= 2010,
  ) %>%
  xtabs(~STATION_CODE + MYEAR + NAME + METHOD_ID, .)
dat_nivabase %>%
  filter(
    NAME %in% c("EROD", "ALAD", "PROTV") & !is.na(VALUE) & MYEAR >= 2010,
  ) %>% pull(SAMPLE_ID) %>% unique() %>% sort()
dat_109 %>%
  filter(
    PARAM %in% c("EROD", "ALAD") & !is.na(VALUE_WW) & MYEAR >= 2010,
  ) %>%
  xtabs(~STATION_CODE + MYEAR + PARAM, .)

check <- readRDS("Input_data/01_df_2022_notstandard_2023-09-12.rds")
check %>%
  filter(
    NAME %in% c("EROD", "ALAD") & !is.na(VALUE),
  ) %>%
  xtabs(~STATION_CODE + NAME, .)


#
# test plot some substances, eider duck ------------------------------------
#
substance <- "D5"
substance <- "HG"
substance <- "PFOS"
substance <- "BDE99"
substances <- c("BDE47", "BDE99", "BDE100")
substances <- c("MCCP eksl. LOQ", "MCCP inkl. LOQ")
ggplot(data = subset(dat_109, MYEAR > 2018 & STATION_CODE == "19N" & PARAM %in% substances),
       aes(MYEAR, VALUE_WW, color = (FLAG1 %in% "<"))) +
  geom_jitter(width = 0.1) +
  scale_y_log10() +
  facet_grid(rows = vars(PARAM), cols = vars(TISSUE_NAME)) +
  labs(title = paste("Eider duck:", paste(substances, collapse = ", ")))
  
