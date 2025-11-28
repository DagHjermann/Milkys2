
# . # . # . # . # . # . # . # . # . # . # . # . # . # . # . # . # . # . # . # . # . # . # . 
# set work directory
# . # . # . # . # . # . # . # . # . # . # . # . # . # . # . # . # . # . # . # . # . # . # . 
setwd("520_Tukey_Kramer_test")
getwd()

source("functions/function_HSDtest.R")
library(shiny)
library(ggplot2)
library(dplyr)

current_year <- 2023

# Data and station metadata
dat <- readRDS("../Data/109_adjusted_data_ELU_2024-09-25.rds")
dat_stations <- read.csv("../Input_data/Lookup_tables/Lookup_stationorder.csv") %>%
  mutate(Station = paste(STATION_CODE, substr(Station_name, 1, 15)))

# no ALAD and EROD for 2024 here:
dat <- readRDS("../Data/109_adjusted_data_ELU_2025-09-01.rds")
dat %>% 
  filter(PARAM %in% c("ALAD", "EROD", "PYR1OH")) %>% 
  xtabs(~PARAM + MYEAR, .)

#
# No data here:
#
dat_80 <- readRDS("../Input_data/80_df_2024_notstandard_ELU_2025-11-12_03h38m.rds")
dat_80 %>% 
  filter(NAME %in% c("ALAD", "EROD", "PYR1OH")) %>% 
  xtabs(~NAME + MYEAR, .)


current_year <- 2024
# Data and station metadata
dat <- readRDS("../Data/109_adjusted_data_ELU_2025-09-01.rds")
dat_stations <- read.csv("../Input_data/Lookup_tables/Lookup_stationorder.csv") %>%
  mutate(Station = paste(STATION_CODE, substr(Station_name, 1, 15)))

c <- 0.1

#
# Add from file
# 
fn <- "../Input_data/Biological_effects_cod/2024/milkys_2024_EROD-ALAD_results.xlsx"
# excel_sheets(fn)
df_raw_erod <- read_excel(fn, sheet = "EROD") %>% 
  mutate(NAME = "EROD") %>% 
  select(STATION_CODE, SPECIMEN_NO = Sampleno, NAME, VALUE = `EROD (pmol/min/mg protein)`)
df_raw_alad <- read_excel(fn, sheet = "ALA-D") %>% 
  mutate(NAME = "ALAD") %>% 
  select(STATION_CODE, SPECIMEN_NO = Sampleno, NAME, VALUE = ALAD)

dat_alad_erod_2024 <- bind_rows(df_raw_erod, df_raw_alad) %>% 
  mutate(
    LATIN_NAME = "Gadus morhua",
    TISSUE_NAME = case_when(
      NAME == "ALAD" ~ "Blod",                # must use exactly this name (from Nivabasen BIOTA_TISSUE_TYPES)
      NAME == "EROD" ~ "Liver - microsome"    # must use exactly this name (from Nivabasen BIOTA_TISSUE_TYPES),
    ),
    FLAG1 = as.character(NA),
    SPECIMEN_NO = as.character(SPECIMEN_NO)
  ) %>% 
  rename(
    PARAM = NAME,
    VALUE_WW = VALUE)

dat2 <- dat %>%
  bind_rows(dat_alad_erod_2024) %>% 
  filter((VALUE_WW + c) > 0) %>%
  left_join(dat_stations %>% select(STATION_CODE, Order, Station), by = "STATION_CODE") %>%
  mutate(
    Matrix = paste0(LATIN_NAME, ", ", TISSUE_NAME),
    log_Conc = log10(VALUE_WW + c),
    LOQ = ifelse(is.na(FLAG1), "Over LOQ", "Under LOQ")
  ) %>%
  filter(!is.na(log_Conc))

matrices <- dat2 %>%
  filter(MYEAR %in% (current_year-5):current_year) %>%
  count(Matrix) %>%
  arrange(desc(n)) %>%
  pull(Matrix)

years <- sort(unique(dat2$MYEAR))


#
# from script 80 ----
#

library(dbplyr)
con <- DBI::dbConnect(odbc::odbc(),
                      Driver = "/usr/lib/oracle/23/client64/lib/libsqora.so.23.1",
                      DBQ = "dbora-niva-prod01.niva.corp:1555/NIVABPRD",
                      UID = rstudioapi::askForPassword("Database username (NIVA initials, 3 capital letters)"),
                      PWD = rstudioapi::askForPassword("Nivabasen password")
)

test_connection <- DBI::dbGetQuery(con, "select * from NIVADATABASE.PROJECTS where rownum < 4")
if ("test_connection" %in% ls()){
  if (nrow(test_connection) == 3)
    cat("Connection to Nivadatabase set up and tested\n")
}
rm(test_connection)



# Remove ENTERED_BY and ENTERED_DATE from all tables because they mess up the joins

methods <- tbl(con, in_schema("NIVADATABASE", "METHOD_DEFINITIONS")) %>%
  select(METHOD_ID, NAME, UNIT, LABORATORY, METHOD_REF, MATRIX_ID)
measurements <- tbl(con, in_schema("NIVADATABASE", "BIOTA_CHEMISTRY_VALUES")) %>%
  select(SAMPLE_ID, METHOD_ID, VALUE_ID, VALUE, FLAG1, 
         DETECTION_LIMIT, UNCERTAINTY, QUANTIFICATION_LIMIT)
# drop STATION_ID, TAXONOMY_CODE_ID from samples
samples <- tbl(con, in_schema("NIVADATABASE", "BIOTA_SAMPLES")) %>%
  select(SAMPLE_ID, TISSUE_ID, SAMPLE_NO, REPNO, REMARK) %>%
  rename(REMARK_sample = REMARK)
samples_specimens <- tbl(con, in_schema("NIVADATABASE", "BIOTA_SAMPLES_SPECIMENS")) %>%
  select(SPECIMEN_ID, SAMPLE_ID)
specimens <- tbl(con, in_schema("NIVADATABASE", "BIOTA_SINGLE_SPECIMENS")) %>%
  select(STATION_ID, SPECIMEN_ID, SPECIMEN_NO, DATE_CAUGHT, TAXONOMY_CODE_ID, REMARK) %>%
  rename(REMARK_specimen = REMARK)
project_stations <- tbl(con, in_schema("NIVADATABASE", "PROJECTS_STATIONS")) %>%
  select(STATION_ID, STATION_CODE, STATION_NAME, PROJECT_ID )
projects <- tbl(con, in_schema("NIVADATABASE", "PROJECTS")) %>%
  select(PROJECT_ID, PROJECT_NAME, PROJECT_DESCRIPTION)

# Lookup tables  
taxonomy_codes <- tbl(con, in_schema("NIVADATABASE", "TAXONOMY_CODES")) %>%
  select(TAXONOMY_CODE_ID, CODE, NIVA_TAXON_ID)
taxonomy <- tbl(con, in_schema("NIVADATABASE", "TAXONOMY")) %>%
  select(NIVA_TAXON_ID, LATIN_NAME)
tissue <- tbl(con, in_schema("NIVADATABASE", "BIOTA_TISSUE_TYPES")) %>%
  select(TISSUE_ID, TISSUE_NAME)
lims_id <- tbl(con, in_schema("NIVADATABASE", "LABWARE_BSID")) %>%
  select(BIOTA_SAMPLE_ID, LABWARE_TEXT_ID)

# Not really necessary here:
# matrix <- tbl(con, in_schema("NIVADATABASE", "MATRIX_DEFINITIONS")) %>%
#   select(MATRIX_ID, MATRIX_NAME)
# collect(matrix)

# Specimen level
# - one row per specimen, in cas of poole dsamples there wil be several rows per sample
dat_1 <- specimens %>%
  mutate(
    YEAR = year(DATE_CAUGHT),
    MONTH = month(DATE_CAUGHT),
    MYEAR = case_when(
      MONTH >= 4 ~ YEAR,
      MONTH < 4 ~ YEAR-1)) %>%
  left_join(samples_specimens) %>%
  left_join(samples) %>% 
  left_join(tissue) %>% 
  left_join(lims_id, by = c("SAMPLE_ID" = "BIOTA_SAMPLE_ID")) %>% 
  left_join(measurements) %>%
  left_join(methods) %>%
  left_join(project_stations) %>%
  left_join(projects) %>%
  # filter(PROJECT_NAME %in% c("CEMP_Biota", "MILKYS (2021-2025)"))  %>%
  left_join(taxonomy_codes) %>%
  left_join(taxonomy)  

# Sample level
# - one row per sample, specimen-level info is discarded  
# - assumes that there is only one project, station and MYEAR per sample  
dat_2 <- dat_1 %>%
  # to avoid duplicates when there are several PROJECT_NAME, PROJECT_ID and LABWARE_TEXT_ID for each sample
  # arrange LABWARE_TEXT_ID with the last one first (in order to keep only the last one, see filter sentence)
  arrange(desc(LABWARE_TEXT_ID)) %>% 
  # keep only rows with the last LABWARE_TEXT_ID (group_by + mutate + filter)
  group_by(
    # Main data
    PROJECT_NAME, STATION_CODE, STATION_NAME, LATIN_NAME, MYEAR, TISSUE_NAME, SAMPLE_NO, REPNO, 
    NAME, VALUE, FLAG1, UNIT, 
    # extra time info
    # DATE_CAUGHT, YEAR, MONTH,
    # extra sample info (note: LABWARE_TEXT_ID not included as there can be two or more TEXT_ID for a single sample)
    REMARK_sample, 
    # extra chemical methiod info
    LABORATORY, METHOD_REF, DETECTION_LIMIT, UNCERTAINTY, QUANTIFICATION_LIMIT, 
    # IDs
    STATION_ID, TAXONOMY_CODE_ID, MATRIX_ID, SAMPLE_ID, TISSUE_ID, METHOD_ID, VALUE_ID) %>% 
  mutate(LABWARE_TEXT_ID_1 = first(LABWARE_TEXT_ID)) %>% 
  ungroup() %>% 
  filter(LABWARE_TEXT_ID == LABWARE_TEXT_ID_1) %>% 
  # avoid rows that differs only by PROJECT_NAME or PROJECT_ID:
  distinct(
    # Main data (PROJECT_NAME removed as it causes duplicates)
    STATION_CODE, STATION_NAME, LATIN_NAME, MYEAR, TISSUE_NAME, SAMPLE_NO, REPNO, 
    NAME, VALUE, FLAG1, UNIT, 
    # extra time info
    # DATE_CAUGHT, YEAR, MONTH,
    # extra sample info (note: LABWARE_TEXT_ID not included as there can be two or more TEXT_ID for a single sample)
    REMARK_sample, 
    # extra chemical methiod info
    LABORATORY, METHOD_REF, DETECTION_LIMIT, UNCERTAINTY, QUANTIFICATION_LIMIT, 
    # IDs
    STATION_ID, TAXONOMY_CODE_ID, MATRIX_ID, SAMPLE_ID, TISSUE_ID, METHOD_ID, VALUE_ID)

#
# find our data
#

test <- samples %>% 
  left_join(tissue) %>% 
  left_join(measurements) %>%
  left_join(methods)  %>% 
  left_join(samples_specimens) %>% 
  left_join(specimens) %>%
  mutate(
    YEAR = year(DATE_CAUGHT),
    MONTH = month(DATE_CAUGHT),
    MYEAR = case_when(
      MONTH >= 4 ~ YEAR,
      MONTH < 4 ~ YEAR-1)) %>%
  count(NAME, VALUE, MYEAR, STATION_ID, TAXONOMY_CODE_ID, MATRIX_ID, SAMPLE_ID, TISSUE_ID, METHOD_ID, VALUE_ID) %>% 
  filter(NAME %in% c("ALAD", "EROD") & MYEAR >= 2022)%>% 
  collect()



df_alad_erod_1 <- dat_2 %>% 
  filter(NAME %in% c("ALAD", "EROD") & MYEAR >= 2022) %>% 
  collect()

xtabs(~NAME + MYEAR, test)
xtabs(~NAME + MYEAR, df_alad_erod_1)

