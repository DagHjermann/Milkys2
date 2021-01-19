
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# This file was created to handle the discovery of an error in the trend code in Nov. 2020
# 
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Packages  
library(dplyr)
library(purrr)
library(lubridate)
library(readxl)

# Functions
source("001_Add_trends_functions.R")  
source("201_Time_series_write_to_Excel_functions.R", encoding = "UTF-8") # for 'make_trend_data_for_excel2'
# source("201_Make_big_excel_file_functions.R")


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#
# 
# 1. Discovery of the error ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#

# Check
# No long-term trend for this, that seems pretty crazy!
model_from_medians("HG", "Gadus morhua", "Muskel", "30B", "DW", 1980:2018, data_med)$statistics

# It turned out that the linear p-value was <0.001 while the non-linear was >0.99
# The p-value for the non-linear was essentially closer to 1-p, which eads to errors when
#   AIC selects the non-linear model as the best
# (Note: in some cases with "s-formed" time trends, such a result MAY be legit.)

if (FALSE){
  # Debugging
  debugonce(calc_models_one_station2)
  debugonce(calc_models_gam)
  debugonce(GAM_trend_analysis)
  debugonce(statistics_for_excel)
  model_from_medians("HG", "Gadus morhua", "Muskel", "30B", "DW", 1980:2018, data_med)$statistics
}

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#
# 
# 2. Error and correction ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#

# Error: is in function 'statistics_for_excel'
# Line 414 in '001_Add_trends_functions.R'  
#    gam_p <- pt((x2-x1)/mean_SE, nrow(regr_results$mod_nonlin$yFit))
# corrected to:
#    gam_p <- 2*(1 - pt(abs(x2-x1)/(1.5*mean_SE), n_fit))
# (the 1.5 factor is there to make this test a bit more conservative, otherwise
#  you get a lower P for GAMs that are selected to be linear than you get with 'lm')

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#
# 
# 3. Re-estimation, 2019 trends ---- 
#
# After correcting the 01 script, we re-estimated trends for 
# 'df_retest_2019' as defined below 
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#

# Read original 'df3_10yr' + 'df3_long' from file
#   df1_10yr <- readRDS("Data/120_result_10yr_2019_2020-08-05.rds")
#   df1_long <- readRDS("Data/120_result_long_2019_2020-08-05.rds")
#
# (since then, we have started using "run01", "run02" etc.)

#
# Check tables  -the ones in the lower left corner are suspicuous  
#
xtabs(~(p_linear < 0.05) + (p_nonlinear < 0.05), 
      df1_10yr %>% filter(Model_used == "Nonlinear" & Basis %in% c("WW","WWa"))
)
xtabs(~(p_linear < 0.05) + (p_nonlinear < 0.05), 
      df1_long %>% filter(Model_used == "Nonlinear" & Basis %in% c("WW","WWa"))
)
#
# Pick those with non-linear models, p < 0.10 for linear and p > 0.05 for non-linear
#
df_retest_10yr <- df1_10yr %>% 
  filter(Model_used %in% "Nonlinear" & !is.na(p_linear) & !is.na(p_nonlinear)) %>%
  filter(p_linear < 0.10 & p_nonlinear > 0.05)
df_retest_long <- df1_long %>% 
  filter(Model_used %in% "Nonlinear" & !is.na(p_linear) & !is.na(p_nonlinear)) %>%
  filter(p_linear < 0.10 & p_nonlinear > 0.05)

# Example - Oslo 30B 
# df_retest_long %>% filter(STATION_CODE %in% "30B" & Basis == "WW") %>% View()

df_retest_10yr %>% nrow()
df_retest_long %>% nrow()

# Should be same as lower left cell in tables above  
df_retest_10yr %>% filter(p_linear < 0.05 & Basis %in% c("WW","WWa")) %>% nrow()
df_retest_long %>% filter(p_linear < 0.05 & Basis %in% c("WW","WWa")) %>% nrow()

df_retest_2019 <- bind_rows(
  df_retest_10yr, df_retest_long,
) %>%
  distinct(PARAM, LATIN_NAME, TISSUE_NAME, STATION_CODE, Basis)

saveRDS(df_retest_2019, "Data/120_df_retest_2019.rds")

#
# Then, script 120 was run with these parameters set in the start 
#   add_extra_series <- TRUE
#   last_year <- 2019
# and in 4c "SPECIAL: add or update trends":
#   df_series <- df_retest_2019
#


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#
# After reanalysis  
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#

df2_10yr <- readRDS("Data/120_result_10yr_2019_2020-08-05.rds")
df2_long <- readRDS("Data/120_result_long_2019_2020-08-05.rds")

# Example - Oslo 30B
df2_long %>% filter(STATION_CODE %in% "30B" & Basis == "WW" & PARAM == "HG")

ggplot(df2_long, aes(p_linear, p_nonlinear, color = Model_used)) + 
  geom_point()

xtabs(~(p_linear < 0.05) + (p_nonlinear < 0.05), 
      df2_10yr %>% filter(Model_used == "Nonlinear" & Basis %in% c("WW","WWa"))
)
xtabs(~(p_linear < 0.05) + (p_nonlinear < 0.05), 
      df2_long %>% filter(Model_used == "Nonlinear" & Basis %in% c("WW","WWa"))
)

df2_long %>% 
  filter(Model_used %in% "Nonlinear" & !is.na(p_linear) & !is.na(p_nonlinear)) %>%
  filter(p_linear < 0.05 & p_nonlinear > 0.05) %>%
  View("odd ones")


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#
# 
# 4. Re-estimation, 2018 trends ---- 
# 
# Similar, for trends until 2018
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#

# Read original 'df3_10yr' + 'df3_long' from file
#   df3_10yr <- readRDS("Data/120_result_10yr_2018_2020-08-05.rds")
#   df3_long <- readRDS("Data/120_result_long_2018_2020-08-05.rds")

#
# Check tables  -the ones in the lower left corner are suspicuous  
#
xtabs(~(p_linear < 0.05) + (p_nonlinear < 0.05), 
      df3_10yr %>% filter(Model_used == "Nonlinear" & Basis %in% c("WW","WWa"))
)
xtabs(~(p_linear < 0.05) + (p_nonlinear < 0.05), 
      df3_long %>% filter(Model_used == "Nonlinear" & Basis %in% c("WW","WWa"))
)

#
# Pick those with non-linear models, p < 0.10 for linear and p > 0.05 for non-linear (WW, long trends)
#
df_retest_10yr <- df3_10yr %>% 
  filter(Model_used %in% "Nonlinear" & !is.na(p_linear) & !is.na(p_nonlinear)) %>%
  filter(p_linear < 0.10 & p_nonlinear > 0.05)
df_retest_long <- df3_long %>% 
  filter(Model_used %in% "Nonlinear" & !is.na(p_linear) & !is.na(p_nonlinear)) %>%
  filter(p_linear < 0.10 & p_nonlinear > 0.05)

# Example - Oslo 30B
# df_retest_long %>% filter(STATION_CODE %in% "30B" & Basis == "WW") %>% View()

df_retest_10yr %>% nrow()
df_retest_long %>% nrow()

# Should be same as lower left corner in the tables above  
df_retest_10yr %>% filter(p_linear < 0.05 & Basis %in% c("WW","WWa")) %>% nrow()
df_retest_long %>% filter(p_linear < 0.05 & Basis %in% c("WW","WWa")) %>% nrow()

df_retest_2018 <- bind_rows(
  df_retest_10yr, df_retest_long,
) %>%
  distinct(PARAM, LATIN_NAME, TISSUE_NAME, STATION_CODE, Basis)

# Save
saveRDS(df_retest_2018, "Data/120_df_retest_2018.rds")

#
# Then, script 120 was run with these parameers set in the start 
#   add_extra_series <- TRUE
#   last_year <- 2018
# and in 4c "SPECIAL: add or update trends":
#   df_series <- df_retest_2018
#

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#
# After reanalysis  
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#

df4_10yr <- readRDS("Data/120_result_10yr_2018_2020-08-05.rds")
df4_long <- readRDS("Data/120_result_long_2018_2020-08-05.rds")

# Example - Oslo 30B
# df4_long %>% filter(STATION_CODE %in% "30B" & Basis == "WW" & PARAM == "HG")

ggplot(df4_long, aes(p_linear, p_nonlinear, color = Model_used)) + 
  geom_point()

xtabs(~(p_linear < 0.05) + (p_nonlinear < 0.05), 
      df4_10yr %>% filter(Model_used == "Nonlinear" & Basis %in% c("WW","WWa"))
)
xtabs(~(p_linear < 0.05) + (p_nonlinear < 0.05), 
      df4_long %>% filter(Model_used == "Nonlinear" & Basis %in% c("WW","WWa"))
)

df4_long %>% 
  filter(Model_used %in% "Nonlinear" & !is.na(p_linear) & !is.na(p_nonlinear)) %>%
  filter(p_linear < 0.05 & p_nonlinear > 0.05) %>%
  View("odd ones 2018")


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
# 
# 5. Making Recalculated_trends ---- 
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#
# Big excel data
#
fn1 <- "Big_excel_table/Data_xl_2020-08-05_ver07.rds"
fn2 <- "Big_excel_table/Data_xl_2020-08-05_ver08.rds"

# read old and new file  

df1 <- readRDS(file = fn1) %>%
  mutate(TISSUE_NAME = case_when(
    TISSUE_NAME %in% "Liver" ~ "Lever",
    TISSUE_NAME %in% "Muscle" ~ "Muskel",
    TISSUE_NAME %in% "Bile" ~ "Galle",
    TISSUE_NAME %in% "Blood" ~ "Blod",
    TRUE ~ TISSUE_NAME)
  ) %>%
  inner_join(df_recalculate_trends %>% select(-Basis) %>% mutate(Recalc = TRUE))

df2 <- readRDS(file = fn2) %>%
  mutate(TISSUE_NAME = case_when(
    TISSUE_NAME %in% "Liver" ~ "Lever",
    TISSUE_NAME %in% "Muscle" ~ "Muskel",
    TISSUE_NAME %in% "Bile" ~ "Galle",
    TISSUE_NAME %in% "Blood" ~ "Blod",
    TRUE ~ TISSUE_NAME)
  ) %>%
  inner_join(df_recalculate_trends %>% select(-Basis) %>% mutate(Recalc = TRUE))

#
# Combine old and new trends
#
df1_comb <- df2 %>%
  select(PARAM, LATIN_NAME, TISSUE_NAME, STATION_CODE, Basis, Trends.2019) %>%
  inner_join(df1 %>% 
               select(PARAM, LATIN_NAME, TISSUE_NAME, STATION_CODE, Basis, Trends.2019) %>%
               rename(Trends.2019_OLD = Trends.2019),
            by = c("PARAM", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE", "Basis")) %>%
  mutate(CHANGE = Trends.2019_OLD != Trends.2019)
View(df1_comb)
dim(df1_comb)

# How many changed?
xtabs(~CHANGE + Basis, df1_comb)

# View(df1_comb)  

# Save
df1_comb %>% 
  writexl::write_xlsx("Data/120_Recalculated_trends.xlsx")

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
# 
# 6. Making Recalculated_trends2 ---- 
#
# Also add last year's correct trend
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#
# We used script 120... "2018data" to make last year's trend  
#
df_long_2018 <- readRDS("Data/120_result_long_2018data.rds")
df_10yr_2018 <- readRDS("Data/120_result_10yr_2018data.rds")

# Key columns:
cols <- c("PARAM", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE", "Basis")
# debugonce(set_symbol)

# Make data frames with trend symbols
debugonce(make_trend_data_for_excel2)

# Same code as in scr. 201, except that we simply replace 'data_xl' with 'df_long_2018'
# - in 201, this is only for getting the same order
trend_long_for_excel <- make_trend_data_for_excel2(df_long_2018, df_long_2018[,cols])
trend_10yr_for_excel <- make_trend_data_for_excel2(df_10yr_2018, df_long_2018[,cols])

# Combine log + 10yr and prepare even more for Excel
trends_for_excel <- combine_long_and_short_trends_for_excel2(trend_long_for_excel, trend_10yr_for_excel)

# Add to our previous results
df1_comb2 <- df1_comb %>%
  rename(CHANGE_2019_correction = CHANGE) %>%
  left_join(trends_for_excel %>% 
              select(PARAM, LATIN_NAME, TISSUE_NAME, STATION_CODE, Basis, Trend.year) %>%
              rename(Trends.2018 = Trend.year)) %>%
  mutate(Change_2018_2019 = (Trends.2019 != Trends.2018))

df1_comb2 %>% head()

# Save
df1_comb2 %>% 
  writexl::write_xlsx("Data/120_Recalculated_trends2.xlsx")



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
# 
# 7. Making Recalculated_trends3 ---- 
#
# AFTER THE *SECOND* FIXING OF ERRORS
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#
# Big excel data
#
fn1 <- "Big_excel_table/Data_xl_2020-08-05_ver07.rds"   # original, with error
fn2 <- "Big_excel_table/Data_xl_2020-08-05_ver08.rds"   # first fix, fixed upward but destrouted downward trends
fn3 <- "Big_excel_table/Data_xl_2020-08-05_ver11.rds"   # second fix

# read old and new file  

df_list <- list(fn1,fn2, fn3) %>%
  map(function(fn) {
    readRDS(file = fn) %>%
      mutate(TISSUE_NAME = case_when(
        TISSUE_NAME %in% "Liver" ~ "Lever",
        TISSUE_NAME %in% "Muscle" ~ "Muskel",
        TISSUE_NAME %in% "Bile" ~ "Galle",
        TISSUE_NAME %in% "Blood" ~ "Blod",
        TRUE ~ TISSUE_NAME)
      )}
  )

#
# Combine old and new trends
#
df_comb <- df_list[[1]] %>%
  select(PARAM, LATIN_NAME, TISSUE_NAME, STATION_CODE, Basis, Trends.2019, Trends.2018) %>%
  rename(Trends.2019_v07 = Trends.2019, Trends.2018_v07 = Trends.2018) %>%
  full_join(df_list[[2]] %>% 
               select(PARAM, LATIN_NAME, TISSUE_NAME, STATION_CODE, Basis, Trends.2019, Trends.2018) %>%
               rename(Trends.2019_v08 = Trends.2019, Trends.2018_v08 = Trends.2018),
             by = c("PARAM", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE", "Basis")
            ) %>%
  full_join(df_list[[3]] %>% 
              select(PARAM, LATIN_NAME, TISSUE_NAME, STATION_CODE, Basis, Trends.2019, Trends.2018) %>%
              rename(Trends.2019_v11 = Trends.2019, Trends.2018_v11 = Trends.2018),
            by = c("PARAM", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE", "Basis")
            ) %>%
  mutate(
    Change.2019_07_08 = Trends.2019_v07 != Trends.2019_v08,
    Change.2019_07_11 = Trends.2019_v07 != Trends.2019_v11,
    Change.2018_07_08 = Trends.2018_v07 != Trends.2018_v08,
    Change.2018_07_11 = Trends.2018_v07 != Trends.2018_v11
    )
nrow(df_comb)  # 24346

df_comb3 <- df_comb %>%
  filter(Change.2019_07_08 | Change.2019_07_11 | Change.2018_07_08 | Change.2018_07_11)
nrow(df_comb3)  # 150

# Save
df_comb3 %>% 
  writexl::write_xlsx("Data/120_Recalculated_trends3.xlsx")

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
# 
# 8. Making Recalculated_trends4 ---- 
#
# AFTER FIX Tuesday 3.11.2020 - version 14
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#
# Big excel data
#
fn4 <- "Big_excel_table/Data_xl_2020-08-05_ver14.rds"   # second fix

# read old and new files  
df_list[[4]] <- list(fn4) %>%
  map(function(fn) {
    readRDS(file = fn) %>%
      mutate(TISSUE_NAME = case_when(
        TISSUE_NAME %in% "Liver" ~ "Lever",
        TISSUE_NAME %in% "Muscle" ~ "Muskel",
        TISSUE_NAME %in% "Bile" ~ "Galle",
        TISSUE_NAME %in% "Blood" ~ "Blod",
        TRUE ~ TISSUE_NAME)
      )}
  )
df_list[[4]] <- df_list[[4]][[1]]
str(df_list,1)

df_comb_x <- df_comb %>%
  full_join(df_list[[4]] %>% 
              select(PARAM, LATIN_NAME, TISSUE_NAME, STATION_CODE, Basis, Trends.2019, Trends.2018) %>%
              rename(Trends.2019_v14 = Trends.2019, Trends.2018_v14 = Trends.2018),
            by = c("PARAM", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE", "Basis")
  ) %>%
  mutate(
    Change.2019_07_08 = Trends.2019_v07 != Trends.2019_v08,
    Change.2019_07_11 = Trends.2019_v07 != Trends.2019_v11,
    Change.2019_07_14 = Trends.2019_v07 != Trends.2019_v14,
    Change.2019_11_14 = Trends.2019_v11 != Trends.2019_v14,
    Change.2018_07_08 = Trends.2018_v07 != Trends.2018_v08,
    Change.2018_07_11 = Trends.2018_v07 != Trends.2018_v11
  )
nrow(df_comb_x)  # 24346

  filter(Change.2019_07_08 | Change.2019_07_11 | Change.2018_07_08 | Change.2018_07_11)
nrow(df_comb3)  # 150

df_comb4 <- df_comb_x %>%
  filter(Change.2019_07_08 | Change.2019_07_11 | Change.2019_07_14 | Change.2019_11_14 |
           Change.2018_07_08 | Change.2018_07_11)
nrow(df_comb4)  # 150

# Save
df_comb4 %>% 
  writexl::write_xlsx("Data/120_Recalculated_trends4.xlsx")


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#
# 
# 9. Re-estimation, 2016 trends ---- 
# 
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#

load("Data_previous_years/Trend_results/2016/13b7_trend_tables.Rdata")
# creates objects 'trend_table_long', 'trend_table_10yr'

# We save these files under this name in order to replace 
#   some of the lines with recalculated time series  
saveRDS(trend_table_10yr, "Data/120_result_10yr_2016_2020-08-05_run01.rds")
saveRDS(trend_table_long, "Data/120_result_long_2016_2020-08-05_run01.rds")

# For simplicity
df5_10yr <- trend_table_10yr
df5_long <- trend_table_long

# Read original 'df3_10yr' + 'df3_long' from file
#   df3_10yr <- readRDS("Data/120_result_10yr_2018_2020-08-05.rds")
#   df3_long <- readRDS("Data/120_result_long_2018_2020-08-05.rds")

#
# Check tables  -the ones in the lower left corner are suspicuous  
#
xtabs(~(p_linear < 0.05) + (p_nonlinear < 0.05), 
      df5_10yr %>% filter(Model_used == "Nonlinear" & Basis %in% c("WW","WWa"))
)
xtabs(~(p_linear < 0.05) + (p_nonlinear < 0.05), 
      df5_long %>% filter(Model_used == "Nonlinear" & Basis %in% c("WW","WWa"))
)

#
# Pick those with non-linear models, p < 0.10 for linear and p > 0.05 for non-linear (WW, long trends)
#
df_retest_10yr <- df5_10yr %>% 
  filter(Model_used %in% "Nonlinear" & !is.na(p_linear) & !is.na(p_nonlinear)) %>%
  filter(p_linear < 0.10 & p_nonlinear > 0.05)
df_retest_long <- df5_long %>% 
  filter(Model_used %in% "Nonlinear" & !is.na(p_linear) & !is.na(p_nonlinear)) %>%
  filter(p_linear < 0.10 & p_nonlinear > 0.05)

# Example - Oslo 30B - Hg NOT here because the linear model was used  
# df_retest_long %>% filter(STATION_CODE %in% "30B" & Basis == "WW") %>% View()

df_retest_10yr %>% nrow()
df_retest_long %>% nrow()

# Should be same as lower left corner in the tables above  
df_retest_10yr %>% filter(p_linear < 0.05 & Basis %in% c("WW","WWa")) %>% nrow()
df_retest_long %>% filter(p_linear < 0.05 & Basis %in% c("WW","WWa")) %>% nrow()

df_retest_2016 <- bind_rows(
  df_retest_10yr, df_retest_long,
) %>%
  distinct(PARAM, LATIN_NAME, TISSUE_NAME, STATION_CODE, Basis)

# Save
saveRDS(df_retest_2016, "Data/120_df_retest_2016.rds")

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#
#
# Then, script 120 was run with these parameters set in the start 
#   add_extra_series <- TRUE  
#   last_year <- 2016  
# and in 4c "SPECIAL: add or update trends":  
#   df_series <- df_retest_2016  
#
# These results will be saved as the 'run2' files used in the next lines
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#

# Data saved in 'Data' as 
#   120_result_10yr_2016_2020-08-05_run02.rds 
#   120_result_long_2016_2020-08-05_run02.rds 


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#
# Making 'trend_symbols_2016'
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#

# The new trend results we just made  
result_long_new <- readRDS("Data/120_result_long_2016_2020-08-05_run02.rds")
result_10yr_new <- readRDS("Data/120_result_10yr_2016_2020-08-05_run02.rds")

# Key columns:
cols <- c("PARAM", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE", "Basis")
# debugonce(set_symbol)

# Make data frames with trend symbols 
trend_long_for_excel <- make_trend_data_for_excel2(result_long_new, result_long_new[,cols])
trend_10yr_for_excel <- make_trend_data_for_excel2(result_10yr_new, result_10yr_new[,cols])

trend_symbols_2016_new <- full_join(
  trend_long_for_excel[c(cols, "Symbol")] %>% rename(Trend_symbol_long = Symbol),
  trend_10yr_for_excel[c(cols, "Symbol")] %>% rename(Trend_symbol_10yr = Symbol)
) %>%
  mutate(Trend_2016_new = paste0(Trend_symbol_long, "/", Trend_symbol_10yr))

nrow(result_long_new)
nrow(result_10yr_new)
nrow(trend_symbols_2016_new)

# Save
saveRDS(trend_symbols_2016_new, "Data/120_trend_symbols_2016.rds")

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#
# Making 'trend_symbols_2016_compare'
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#

# The old results 
result_long_old <- readRDS("Data/120_result_long_2016_2020-08-05_run01.rds")
result_10yr_old <- readRDS("Data/120_result_10yr_2016_2020-08-05_run01.rds")

# Make data frames with trend symbols 
trend_long_for_excel <- make_trend_data_for_excel2(result_long_old, result_long_old[,cols])
trend_10yr_for_excel <- make_trend_data_for_excel2(result_10yr_old, result_10yr_old[,cols])

trend_symbols_2016_old <- full_join(
  trend_long_for_excel[c(cols, "Symbol")] %>% rename(Trend_symbol_long = Symbol),
  trend_10yr_for_excel[c(cols, "Symbol")] %>% rename(Trend_symbol_10yr = Symbol)
) %>%
  mutate(Trend_2016_old = paste0(Trend_symbol_long, "/", Trend_symbol_10yr))

trend_symbols_2016_compare <- full_join(
  trend_symbols_2016_new %>% select(c(cols, "Trend_2016_new")),
  trend_symbols_2016_old %>% select(c(cols, "Trend_2016_old"))
) 
nrow(trend_symbols_2016_compare)  # 24346

trend_symbols_2016_compare_diff <- trend_symbols_2016_compare %>%
  filter(Trend_2016_new != Trend_2016_old)
nrow(trend_symbols_2016_compare_diff)  # 54

# Save
saveRDS(trend_symbols_2016_compare_diff, "Data/120_trend_symbols_2016_compare_diff.rds")

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#
# 
# 10. Re-estimation, 2017 trends ---- 
# 
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#

trend_table_10yr <- readRDS("Data_previous_years/Trend_results/2017/15_result_10yr_2018-08-23.RData")
trend_table_long <- readRDS("Data_previous_years/Trend_results/2017/15_result_long_2018-08-23.RData")

# We save these files under this name in order to replace 
#   some of the lines with recalculated time series  
saveRDS(trend_table_10yr, "Data/120_result_10yr_2017_2020-08-05_run01.rds")
saveRDS(trend_table_long, "Data/120_result_long_2017_2020-08-05_run01.rds")

# For simplicity
df6_10yr <- trend_table_10yr
df6_long <- trend_table_long

# Read original 'df3_10yr' + 'df3_long' from file
#   df3_10yr <- readRDS("Data/120_result_10yr_2018_2020-08-05.rds")
#   df3_long <- readRDS("Data/120_result_long_2018_2020-08-05.rds")

#
# Check tables  -the ones in the lower left corner are suspicuous  
#
xtabs(~(p_linear < 0.05) + (p_nonlinear < 0.05), 
      df6_10yr %>% filter(Model_used == "Nonlinear" & Basis %in% c("WW","WWa"))
)
xtabs(~(p_linear < 0.05) + (p_nonlinear < 0.05), 
      df6_long %>% filter(Model_used == "Nonlinear" & Basis %in% c("WW","WWa"))
)

#
# Pick those with non-linear models, p < 0.10 for linear and p > 0.05 for non-linear (WW, long trends)
#
df_retest_10yr <- df6_10yr %>% 
  filter(Model_used %in% "Nonlinear" & !is.na(p_linear) & !is.na(p_nonlinear)) %>%
  filter(p_linear < 0.10 & p_nonlinear > 0.05)
df_retest_long <- df6_long %>% 
  filter(Model_used %in% "Nonlinear" & !is.na(p_linear) & !is.na(p_nonlinear)) %>%
  filter(p_linear < 0.10 & p_nonlinear > 0.05)

# Example - Oslo 30B - Hg NOT here because the linear model was used  
# df_retest_long %>% filter(STATION_CODE %in% "30B" & Basis == "WW") %>% View()

df_retest_10yr %>% nrow()
df_retest_long %>% nrow()

# Should be same as lower left corner in the tables above  
df_retest_10yr %>% filter(p_linear < 0.05 & Basis %in% c("WW","WWa")) %>% nrow()
df_retest_long %>% filter(p_linear < 0.05 & Basis %in% c("WW","WWa")) %>% nrow()
# 1
# 74

df_retest_2017 <- bind_rows(
  df_retest_10yr, df_retest_long,
) %>%
  distinct(PARAM, LATIN_NAME, TISSUE_NAME, STATION_CODE, Basis)

# Save
saveRDS(df_retest_2017, "Data/120_df_retest_2017.rds")

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#
#
# Then, script 120 was run with these parameters set in the start 
#   add_extra_series <- TRUE  
#   last_year <- 2017  
# and in 4c "SPECIAL: add or update trends":  
#   df_series <- df_retest_2017  
#
# These results will be saved as the 'run2' files used in the next lines
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#
# Making 'trend_symbols_2017'
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#

# The new trend results we just made  
result_long_new <- readRDS("Data/120_result_long_2017_2020-08-05_run02.rds")
result_10yr_new <- readRDS("Data/120_result_10yr_2017_2020-08-05_run02.rds")

# Key columns:
cols <- c("PARAM", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE", "Basis")
# debugonce(set_symbol)

# Make data frames with trend symbols 
trend_long_for_excel <- make_trend_data_for_excel2(result_long_new, result_long_new[,cols])
trend_10yr_for_excel <- make_trend_data_for_excel2(result_10yr_new, result_10yr_new[,cols])

trend_symbols_2017_new <- full_join(
  trend_long_for_excel[c(cols, "Symbol")] %>% rename(Trend_symbol_long = Symbol),
  trend_10yr_for_excel[c(cols, "Symbol")] %>% rename(Trend_symbol_10yr = Symbol)
)
nrow(trend_symbols_2017_new)
trend_symbols_2017_new <- trend_symbols_2017_new %>%
  mutate(Trend_2017_new = paste0(Trend_symbol_long, "/", Trend_symbol_10yr))

nrow(result_long_new)
nrow(result_10yr_new)
nrow(trend_symbols_2017_new) # 8574

# Save
saveRDS(trend_symbols_2017_new, "Data/120_trend_symbols_2017.rds")

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#
# Making 'trend_symbols_2017_compare'
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#

# The old results 
result_long_old <- readRDS("Data/120_result_long_2017_2020-08-05_run01.rds")
result_10yr_old <- readRDS("Data/120_result_10yr_2017_2020-08-05_run01.rds")

# Make data frames with trend symbols 
trend_long_for_excel <- make_trend_data_for_excel2(result_long_old, result_long_old[,cols])
trend_10yr_for_excel <- make_trend_data_for_excel2(result_10yr_old, result_10yr_old[,cols])

trend_symbols_2017_old <- full_join(
  trend_long_for_excel[c(cols, "Symbol")] %>% rename(Trend_symbol_long = Symbol),
  trend_10yr_for_excel[c(cols, "Symbol")] %>% rename(Trend_symbol_10yr = Symbol)
) %>%
  mutate(Trend_2017_old = paste0(Trend_symbol_long, "/", Trend_symbol_10yr))

trend_symbols_2017_compare <- full_join(
  trend_symbols_2017_new %>% select(c(cols, "Trend_2017_new")),
  trend_symbols_2017_old %>% select(c(cols, "Trend_2017_old"))
) 
nrow(trend_symbols_2017_compare)  # 8574

trend_symbols_2017_compare_diff <- trend_symbols_2017_compare %>%
  filter(Trend_2017_new != Trend_2017_old)
nrow(trend_symbols_2017_compare_diff)  # 91

# Save
saveRDS(trend_symbols_2017_compare_diff, "Data/120_trend_symbols_2017_compare_diff.rds")

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
# 
# 11. Making Recalculated_trends5 ---- 
#
# For 2016 and 2017 as well
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#
# 2018 and 2019: from big excel  
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#

bigxl_old <- readRDS("Big_excel_table/Data_xl_2020-08-05_ver06.rds") %>%
  .[c(cols, "Trends.2018", "Trends.2019")] %>%
  rename(Trend_2018_old = Trends.2018,
         Trend_2019_old = Trends.2019)

bigxl_new <- readRDS("Big_excel_table/Data_xl_2020-08-05_ver15.rds") %>%
  .[c(cols, "Trends.2018", "Trends.2019")] %>%
  rename(Trend_2018_new = Trends.2018,
         Trend_2019_new = Trends.2019)

bigxl_old <- bigxl_old %>%
      mutate(TISSUE_NAME = case_when(
        TISSUE_NAME %in% "Liver" ~ "Lever",
        TISSUE_NAME %in% "Muscle" ~ "Muskel",
        TISSUE_NAME %in% "Bile" ~ "Galle",
        TISSUE_NAME %in% "Blood" ~ "Blod",
        TRUE ~ TISSUE_NAME)
  )
bigxl_new <- bigxl_new %>%
  mutate(TISSUE_NAME = case_when(
    TISSUE_NAME %in% "Liver" ~ "Lever",
    TISSUE_NAME %in% "Muscle" ~ "Muskel",
    TISSUE_NAME %in% "Bile" ~ "Galle",
    TISSUE_NAME %in% "Blood" ~ "Blod",
    TRUE ~ TISSUE_NAME)
  )


if (FALSE){
  # Check 2018-19 data
  check <- full_join(bigxl_old, bigxl_new)
  for (i in 6:9)
    check[[i]] <- symbol_from_text(check[[i]])   # change these to readable symbols  
  check %>%
    filter(PARAM == "HG" & STATION_CODE == "30B" & Basis == "WW") %>%
    View()
}

#
# Combine all!
#
df_comb5 <- full_join(bigxl_old, bigxl_new) %>%
  full_join(trend_symbols_2016_compare) %>%
  full_join(trend_symbols_2017_compare) %>%
  select(PARAM:Basis, 
         Trend_2016_old, Trend_2016_new, Trend_2017_old, Trend_2017_new, 
         Trend_2018_old, Trend_2018_new, Trend_2019_old, Trend_2019_new) %>%
  mutate(Diff2016 = ifelse(Trend_2016_old==Trend_2016_new, 0 , 1),     # 1 if trend is affected by code error
         Diff2017 = ifelse(Trend_2017_old==Trend_2017_new, 0 , 1),
         Diff2018 = ifelse(Trend_2018_old==Trend_2018_new, 0 , 1),
         Diff2019 = ifelse(Trend_2019_old==Trend_2019_new, 0 , 1)
  ) %>%
  filter(Diff2016 + Diff2017 + Diff2018 + Diff2019 > 0)


if (FALSE){
  
  # Check 2016-19 data
  check <- df_comb5
  for (i in 6:13)
    check[[i]] <- symbol_from_text(check[[i]])  # change these to readable symbols  
  check %>%
    filter(PARAM == "HG" & STATION_CODE == "30B" & Basis == "WW") %>%
    View()
}

# Save

list_for_save <- list(
  Data = df_comb5,
  Info = data.frame(
    Info = "For 2018-2019, 'old' = version 6 and 'new' = version 15")
  )

writexl::write_xlsx(list_for_save, "Data/120_Recalculated_trends5.xlsx")




# . ----


