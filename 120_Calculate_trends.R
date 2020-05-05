
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
# 
# Calculate trends for all series
#
# Ca. 20 minutes run time  
#
# NOTE: this does not add to the data file (data_med), it instead creates two data.frames
#   result_10yr and result_long
# If you want to add or replace a few time series, go to the "SPECIAL" part
#   below, set add_extra_series to TRUE, and modify the filtering of df_series.
#   The "Save" part automatically reads the old data and adds/replaces the new series
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#  
## Libraries ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

library(dplyr)
library(purrr)
library(lubridate)
library(openxlsx)
library(mgcv)
library(AICcmodavg)   # AICc()
library(ggplot2)
source("001_Add_trends_functions.R")  # Copied from '16_Trend_functions.R' in Milkys_2018

#
# Define a couple of extra functions for shortening code
#
get_stats <- function(df){
  calc_models_one_station2(df, gam = TRUE, log = TRUE)$statistics_for_file
}

model_from_medians_stat <- function(...)
  model_from_medians(...)$statistics


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#  
## Data ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Medians
# Read and reformat the most recent data (by default)  
files <- dir("Data", pattern = "110_mediandata_updated") %>% rev()

cat("Reading the last file downloaded:")
cat("\n", files[1])
cat("\n")
cat("If you want to read a different file, replace 'files[1]' with the file you want")
cat("\n")

filename <- paste0("Data/", files[1]) 
data_med <- readRDS(filename) %>%
  rename(Proref_median = Median,
         Median = Value) 

# We save the date part of the text (e.g., '2020-04-23')
# This will be used in part 10, when we save the resulting file
file_date <- substr(filename, 29, 38)    # pick out the part of the text from character no. 17 to no. 26


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#  
## Substance groups ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Substance groups
df_substancegroups <- read.xlsx("Input_data/Lookup table - substance groups.xlsx")


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#  
## Test model_from_medians for single series ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# debugonce(model_from_medians)

if (FALSE){
  model_from_medians("TBT", "Nucella lapillus", "Whole soft body", "11G", "DW", 1980:2018, data_med)$statistics
  model_from_medians("TBT", "Nucella lapillus", "Whole soft body", "227G1", "DW", 1980:2018, data_med)$statistics
  model_from_medians("HG", "Gadus morhua", "Muskel", "30B", "DW", 1980:2018, data_med)$statistics
  model_from_medians("HG", "Gadus morhua", "Muskel", "30B", "WW", 2009:2018, data_med)$statistics
  model_from_medians("CB_S7", "Gadus morhua", "Lever", "30B", "WW", 2009:2018, data_med)$statistics
  model_from_medians("TBT", "N. lapillus / L. littorea", "Whole soft body", "71G", "WW", 2009:2018, data_med)$statistics
  model_from_medians("VDSI/Intersex", "N. lapillus / L. littorea", "Whole soft body", "71G", "WW", 1980:2018, data_med)$statistics
}

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#  
## Substances ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

sel <- with(df_substancegroups,
            !is.na(Parameter_group) &
              Parameter_group != "Dioxins" & 
              (Parameter_group != "TBT (tinnorganisk)" | PARAM == "TBT")
)
pars <- df_substancegroups$PARAM[sel]

#
# Check if we have all sum variables (we don't) 
#
# c("CB_S7", "BDE6S", "P_S", "PFAS", "HBCDD", "BDESS", "PAH16", "KPAH", "DDTEP") %in% pars
# [1] FALSE FALSE  TRUE FALSE FALSE FALSE  TRUE  TRUE

# Add sum variables and VDSI/Intersex
pars <- c(pars, c("CB_S7", "BDE6S", "PFAS", "HBCDD", "BDESS", "VDSI/Intersex", "TPTIN"))


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#  
## Make df ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

df <- data_med %>%
  filter(!is.na(Median) & PARAM %in% pars)

# xtabs(~Basis, df)

# filter(!is.na(Median) & PARAM %in% pars & Basis %in% c("WW"))

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#  
## Make df_series ----
#  Based on df
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

lastyear <- 2017   # last year of series must be this year or later 
                   # We need t have data in one of the three last years

df_series <- df %>%
  filter(Basis %in% c("WW","WWa","DW","DWa","FB","FBa")) %>%
  group_by(STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM, UNIT, Basis) %>%
  summarise(No_of_years = n(), Last_year = max(MYEAR)) %>%
  filter(No_of_years >= 4 & Last_year >= lastyear) %>%
  ungroup()

cat("Number of series:", nrow(df_series), "\n")
cat("Number of minutes:", nrow(df_series)/174*20/60, "\n")   # as 174 series took 20 seconds

# xtabs(~Basis, df_series)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#  
## Test procedure ----
#   Using a sample of 20 series, takes just a few seconds
#
# NOTE: This procedure generates a lot of error messages,
#   even when it functions normally
#
# The "usual" error messages is the one below, meaning that GAM (non-linear regression)
#   failed, probably because there was too little data:
#    "Error in smooth.construct.tp.smooth.spec(object, dk$data, dk$knots) : 
#     A term has fewer unique covariate combinations than specified maximum degrees of freedom"
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

df_series_random <- df_series %>% 
  sample_n(20)        # 20 random series
  
# Turn into list
args <- df_series_random[1:100,] %>% 
  as.list()
# names(args) 

# Change names of the list objects, so they fit with input names of 'model_from_medians'
names(args) <- sub("STATION_CODE", "station", names(args))
names(args) <- sub("PARAM", "param", names(args))
names(args) <- sub("LATIN_NAME", "species", names(args))
names(args) <- sub("TISSUE_NAME", "tissue", names(args))
names(args) <- sub("Basis", "basis", names(args))
args <- args[c("station", "tissue", "species", "param", "basis")]


# options()$show.error.messages

# FOR DEBUGGING ONLY:
# debugonce(model_from_medians)  
# debugonce(calc_models_one_station2)
# debugonce(calc_models_gam)
# debugonce(GAM_trend_analysis)
# debugonce(statistics_for_excel)

# Turn off error messages if you want
# old_options <- options(show.error.messages = FALSE, warn = -1)

# Run models
result_list_10yr <- args %>% pmap(model_from_medians_stat, yrs = 2010:2019, data_medians = data_med)
result_list_long <- args %>% pmap(model_from_medians_stat, yrs = 1980:2019, data_medians = data_med)

# Turn error messages on again
# options(old_options)

# Combine lists to data frames
result_10yr <- bind_rows(result_list_10yr)
result_long <- bind_rows(result_list_long)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# SPECIAL: getting and adding trends for one/a few parameters/stations ----
#
# Two examples here: 
#   'VDSI/Intersex' added to existing trends
#   'TPTIN' to existing trends
#
# Note: if 'add_extra_series' is (correctly) set to TRUE, the 'save results' part
#   will automatically read the existing (last saved) trends, add new ones, 
#   and re-save the trends. If the new series made her overlap with old ones, 
#   the old ones will be deleted before the new ones are added, avoiding duplication
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Set 'add_extra_series' to TRUE if you want to add just add a few trends to existing trends
#   or if you want to replace a few trends
# (Will also be used futher below, in the 'save' part)
add_extra_series <- TRUE
add_extra_series <- FALSE

# Then change the filter below in order to do trends for just selected series
# I.e., we shorten df_series drastically

# Used for 'VDSI/Intersex'
# if (add_extra_series){
#   df_series <- df_series %>% 
#     filter(PARAM %in% "VDSI/Intersex" & Basis %in% "WW") 
#   } 

# Used for 'TPTIN'
if (add_extra_series){
  df_series <- df_series %>% 
    filter(PARAM %in% "TPTIN") 
} 

nrow(df_series)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#  
## Full procedure ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

args <- df_series %>% 
  as.list()
# names(args) 

# Change names of the list objects, so they fit with input names of 'model_from_medians'
names(args) <- sub("STATION_CODE", "station", names(args))
names(args) <- sub("PARAM", "param", names(args))
names(args) <- sub("LATIN_NAME", "species", names(args))
names(args) <- sub("TISSUE_NAME", "tissue", names(args))
names(args) <- sub("Basis", "basis", names(args))
args <- args[c("station", "tissue", "species", "param", "basis")]


model_from_medians_stat_s <- safely(model_from_medians_stat)
  
# Procedure
t0 <- Sys.time()     # Start time for procedure
zz <- file("temp.txt", open = "wt")    # Makes file for writing error messages
sink(zz, type = "message")
result_list_10yr <- args %>% pmap(model_from_medians_stat_s, yrs = 2010:2019, data_medians = data_med)
result_list_long <- args %>% pmap(model_from_medians_stat_s, yrs = 1980:2019, data_medians = data_med)
sink()  
Sys.time() - t0  # 36.55274 minutes

# Transpose lists
# Each list is a list of 2 elements: $result and $error 
result_list_10yr_t <- transpose(result_list_10yr)
result_list_long_t <- transpose(result_list_long)

# Combine list 1 to data frame
no_error <- is.null(result_list_10yr_t$error)
result_10yr <- bind_rows(result_list_10yr_t$result[no_error])

# Combine list 2 to data frame
no_error <- is.null(result_list_long_t$error)
result_long <- bind_rows(result_list_long_t$result[no_error])


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#  
## Save results  ----
#
# Note: if 'add_extra_series' is set to TRUE, this procedure will
#   just add the new series to the old ones. See 'SPECIAL' above
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#
# Make backups with time stamp
#

# Timestamp on format "2019-10-18 16-32-53"
timestamp <- gsub(':', '-', substr(Sys.time(),1,19), fixed = TRUE)

file.copy("Data/120_result_10yr.RData", 
          paste0("Data/120_result_10yr_back", timestamp,".RData"))
file.copy("Data/120_result_long.RData", 
          paste0("Data/120_result_long_back", timestamp,".RData"))

#
# For reading newest backed-up file 
#
if (FALSE){
  
  fn <- get_newest_filenames("120_result_long.+back.+rda", "back_", ".rda")
  result_long <- readRDS(paste0("Data/", fn))

  fn <- get_newest_filenames("120_result_10yr.+back.+rda", "back_", ".rda")
  result_10yr <- readRDS(paste0("Data/", fn))
  
}


if (!add_extra_series){

  #
  # In the normal case - if we are making trends for all series
  #
  saveRDS(result_10yr, "Data/120_result_10yr.RData")
  saveRDS(result_long, "Data/120_result_long.RData")

} else if (add_extra_series){

  #
  # Will be run only if we are adding series to existing (saved) series 
  #   ('original' below), or replacing existing series 
  #

  # Read saved 10 year series...
  result_10yr_original <- readRDS("Data/120_result_10yr.RData") %>%
    # Remove all series already present in result_10yr:
    safe_anti_join(result_10yr, 
                   by = c("STATION_CODE", "PARAM", "LATIN_NAME", "TISSUE_NAME", "Basis"),
                   check = "BCUV")

  # Read saved 'long' series...
  result_long_original <- readRDS("Data/120_result_long.RData") %>%
    # Remove all series already present in result_long:
    safe_anti_join(flag_replace_long, 
                 by = c("STATION_CODE", "PARAM", "LATIN_NAME", "TISSUE_NAME", "Basis"),
                 check = "BCV")
  
  # Add the newly made series
  result_10yr <- bind_rows(result_10yr_original, result_10yr)
  result_long <- bind_rows(result_long_original, result_long)
  
  # Check that series are unique
  check1 <- result_long_original %>%
    count(STATION_CODE, PARAM, LATIN_NAME, TISSUE_NAME, Basis) %>%
    filter(n > 1) %>%
    nrow()

  check2 <- result_10yr_original %>%
    count(STATION_CODE, PARAM, LATIN_NAME, TISSUE_NAME, Basis) %>%
    filter(n > 1) %>%
    nrow()

  if (check1 == 0 & check2 == 0){
    cat("Series are unique\n")
  } else {
    cat("WARNING: Series are not unique!\n")
  }

  saveRDS(result_10yr, "Data/120_result_10yr.RData")
  saveRDS(result_long, "Data/120_result_long.RData")

}
 


 
xtabs(~Basis, result_10yr)


check <- FALSE
if (check){
  result_10yr <- readRDS("Data/120_result_10yr.RData")
  result_long <- readRDS("Data/120_result_long.RData")

  xtabs(~PARAM, result_10yr)
  xtabs(~STATION_CODE, result_10yr %>% filter(PARAM %in% "CB118"))
  result_10yr %>% 
    filter(PARAM %in% "CB118" & STATION_CODE %in% "30B")
  result_long %>% 
    filter(PARAM %in% "CB118" & STATION_CODE %in% "30B")

  }


