
# Packages
source("001_Add_trends_functions.R")  
source("201_Time_series_write_to_Excel_functions.R", encoding = "UTF-8")


# Check
# No long-term trend, that seems pretty crazy!
model_from_medians("HG", "Gadus morhua", "Muskel", "30B", "DW", 1980:2018, data_med)$statistics

if (FALSE){
  # Debugging
  debugonce(calc_models_one_station2)
  debugonce(calc_models_gam)
  debugonce(GAM_trend_analysis)
  debugonce(statistics_for_excel)
  model_from_medians("HG", "Gadus morhua", "Muskel", "30B", "DW", 1980:2018, data_med)$statistics
  # Error: is in function 'statistics_for_excel'
  # Line 414 in '001_Add_trends_functions.R'  
  #    gam_p <- pt((x2-x1)/mean_SE, nrow(regr_results$mod_nonlin$yFit))
  # should be:
  #    gam_p <- 1 - pt((x2-x1)/mean_SE, nrow(regr_results$mod_nonlin$yFit))
}

# Which model was used?
xtabs(~Model_used, result_long)
xtabs(~Model_used, result_10yr)

# For non-linear models, how was p for linear and non-linear?
xtabs(~(p_linear < 0.05) + (p_nonlinear < 0.05), result_long %>% filter(Model_used == "Nonlinear" & Basis == "WW"))

# Pick thos with non-linear models, p < 0.05 for linear and p > 0.05 for non-linear (WW, long trends)
df1 <- result_long %>% 
  filter(Model_used %in% "Nonlinear" & Basis %in% "WW" & !is.na(p_linear) & !is.na(p_nonlinear)) %>%
  filter(p_linear < 0.05 & p_nonlinear > 0.05)
nrow(df1)
View(df1)

# Same, for 10-year trends
df2 <- result_10yr %>% 
  filter(Model_used %in% "Nonlinear" & Basis %in% "WW" & !is.na(p_linear) & !is.na(p_nonlinear)) %>%
  filter(p_linear < 0.05 & p_nonlinear > 0.05)
nrow(df2)
View(df2)

# Write to excel
writexl::write_xlsx(bind_rows(df1, df2), "Data/120_Dubious_trends.xlsx")

# For script 120  
df_recalculate_trends <- bind_rows(df1, df2) %>%
  distinct(PARAM, LATIN_NAME, TISSUE_NAME, STATION_CODE, Basis)

# saveRDS(df_recalculate_trends, "Data/120_df_recalculate_trends.rds")
# df_recalculate_trends <- readRDS("Data/120_df_recalculate_trends.rds")

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# AFTER RUNNING THESE SERIES AGAIN in 120 and also run 201 again
# -> new version of big Excel: Data_xl_2020-08-05_ver08.rds
#
# Comparison below
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




