
#
# 173
#

library(dplyr)
library(ggplot2)

#  "172_Read_cod_bioleffect_data.Rmd"
# "917_Check_PAH_metabolites.R"

dat_109 <- readRDS("Data/109_adjusted_data_ELU2_2023-09-12.rds")
dat_109 %>%
  filter(STATION_CODE == "15B" & grepl("pyr", PARAM, ignore.case = T)) %>%
  xtabs(~MYEAR + PARAM, .)  
dat_109 %>%
  filter(STATION_CODE == "15B" & PARAM %in% c("AY", "AY380", "ABS380")) %>%
  xtabs(~MYEAR + PARAM, .)  

# Create data for 2001-2004 (we skip the former ones)  

dat_109_to_add_1 <- dat_109 %>%
  filter(STATION_CODE == "15B" & PARAM %in% "PYR1O" & MYEAR %in% c(2001:2004, 2008:2014)) %>%
  mutate(
    PYR1O = VALUE_WW,
    PARAM = "PYR1OH",
    VALUE_WW = NA)
dat_109_absorbance <- dat_109 %>%
  filter(STATION_CODE == "15B" & PARAM %in% c("AY", "AY380", "ABS380") & MYEAR %in% c(2001:2004, 2008:2014)) %>%
  select(STATION_CODE, MYEAR, SAMPLE_NO2, VALUE_WW) %>%
  rename(AY380 = VALUE_WW)
nrow(dat_109_to_add_1)
nrow(dat_109_absorbance)
dat_109_to_add <- dat_109_to_add_1 %>%
  left_join(dat_109_absorbance, join_by(STATION_CODE, SAMPLE_NO2, MYEAR), relationship = "many-to-one") %>%
  # Make unnormalized data (also see scr. 172):
  mutate(VALUE_WW = PYR1O*AY380)
nrow(dat_109_to_add)

# Add to original data
dat_109_new <- dat_109 %>%
  bind_rows(dat_109_to_add %>% select(-AY380, -PYR1O))
dim(dat_109)
dim(dat_109_new)

# Checking 
dat_sel <- dat_109_new %>%
  filter(STATION_CODE == "15B" & PARAM %in% "PYR1OH")
xtabs(~MYEAR + PARAM, dat_sel)  
ggplot(dat_sel, aes(MYEAR, VALUE_WW)) +
  geom_smooth( )+
  geom_point()  

# Medians
dat_sel_median <- dat_sel %>%
  group_by(MYEAR) %>%
  summarize(
    VALUE_WW = median(VALUE_WW),
    Over_loq = mean(is.na(FLAG1)))
ggplot(dat_sel_median, aes(MYEAR, VALUE_WW)) +
  geom_smooth( )+
  geom_point()  


# tar_target(filename, paste0("data/work/109_split/109_", group, ".rds")),
# tar_target(file, filename, format = "file"),
# tar_target(data1, readRDS(file)),
# tar_target(data2, homogenize_series(data1)),
# tar_target(data3, lc_prepare2(data2)),
# tar_target(series1, create_series_for_flag(data3, last_year)),
# tar_target(data4, add_rule1(series1, data3)),
# tar_target(data5, add_rule2(series1, data4)),
# tar_target(series2, create_series_with_counts(data5, last_year)),
# tar_target(series3, create_series_with_trendtype(series2, last_year)),
# tar_target(result, get_trend_results(series3, data5))

source("../Milkys3_test/R/functions_trend.R")
library(leftcensored)
data3 <- lc_prepare(dat_sel, x = "MYEAR",
                             y = "VALUE_WW", 
                             censored = "FLAG1",
                             log = TRUE,
                             keep_original_columns = TRUE)
# lc_plot(data3)
series1 <- create_series_for_flag(data3, last_year = 2022)
data4 <- add_rule1(series1, data3)
data5 <- add_rule2(series1, data4)
series2 <- create_series_with_counts(data5, last_year = 2022)
series3 <- create_series_with_trendtype(series2, last_year = 2022)

ggplot(data5, aes(x, exp(y))) +
  geom_smooth( )+
  geom_point() 

# Run regression 
# result <- get_trend_results(series3, data5)

# Save data and results
# saveRDS(series3, "Data/173_PYR1OH_15B_series3.rds")
# saveRDS(data5, "Data/173_PYR1OH_15B_data5.rds")
# saveRDS(result, "Data/173_PYR1OH_15B_result.rds")

result <- readRDS("Data/173_PYR1OH_15B_result.rds")

str(result, 1)
str(result, 2)
str(result[[1]]$result, 1)
k_sel <- result[[1]]$result$k_sel

plot_data <- result[[1]]$result$plot_data[[k_sel]] %>%
  mutate(
    VALUE_WW = exp(y),
    VALUE_WW_lo = exp(y_q2.5),
    VALUE_WW_up = exp(y_q97.5)
  )

diff_data_20 <- result[[1]]$result$diff_data[[k_sel]] %>% filter(x == 2002)
diff_data_10 <- result[[1]]$result$diff_data[[k_sel]] %>% filter(x == 2012)
annual_perc_change_20 <- round((exp(-diff_data_10$y_mean/20)-1)*100, 1)
annual_perc_change_10 <- round((exp(-diff_data_10$y_mean/10)-1)*100, 1)
trendstring_comb <- paste0(
  "Change per year since 2002: ", annual_perc_change_20, "%", "\n",
  "Change per year since 2012: ", annual_perc_change_10, "%")
trendstring_x <- 2013

ospar_bac <- 21  
      
ggplot(plot_data, aes(x, VALUE_WW)) +
  geom_ribbon(aes(ymin = VALUE_WW_lo, ymax = VALUE_WW_up), fill = "lightblue") +
  geom_path() +
  geom_point(data = dat_sel_median, aes(x=MYEAR, y = VALUE_WW))
  
gg <- ggplot(plot_data %>% filter(x >= 2002), 
       aes(x, VALUE_WW)) +
  geom_ribbon(aes(ymin = VALUE_WW_lo, ymax = VALUE_WW_up), fill = "lightblue") +
  geom_path() +
  geom_point(data = dat_sel_median %>% filter(MYEAR >= 2002), 
             aes(x=MYEAR, y = VALUE_WW)) +
  annotate("text", x = trendstring_x, y = Inf, label = trendstring_comb, hjust = 0, vjust = 1.2, size = 4, colour = "blue3") +
  geom_hline(yintercept = ospar_bac, linetype = 2, size = 1, colour = "orange") +
  annotate("text", x = 2002, y = ospar_bac, label = "BAC", hjust = 0, vjust = -0.4, size = 4, colour = "orange") +
  theme_bw() +
  labs(y = expression(Concentration*","*~mu*g/kg~(w.w.))) +
  theme(axis.title.x = element_blank())

gg
ggsave("")




#
# APPENDIX ----
#

if (FALSE){
  
  # CHECK DATA USED IN APP LAST YEAR
  # From last year's app version (see commit 3320625c, 2023-10-19)
  # folder_results <- "../Data/125_results_2021_07"
  # Those doesn't seem to contain PYR1OH in 15B : 
  
  # 1
  dat_all_prep3 <- readRDS("Data/125_results_2021_07_input/125_dat_all_prep3.rds")
  dat_all_prep3 %>% filter(STATION_CODE == "15B") %>% xtabs(~PARAM, .)  
  # 2
  dat_series_trend <-  readRDS("Data/125_results_2021_07_input/125_dat_series_trend.rds") 
  dat_series_trend %>% filter(STATION_CODE == "15B") %>% xtabs(~PARAM, .)
  # 3
  df_trend <-  readRDS("Data/125_results_2021_07_output/126_df_trend_2021.rds") 
  df_trend %>% filter(STATION_CODE == "15B") %>% xtabs(~PARAM, .)
  
  
  # CHECK RAW DATA FROM LAST YEAR
  # Contains normalised data (PYR1O) and absorbace (AY etc), but not PYR1OH (non-normalised)   
  dat_109_old %>%
    filter(STATION_CODE == "15B" & grepl("pyr", PARAM, ignore.case = T)) %>%
    xtabs(~MYEAR + PARAM, .)  
  dat_109_old %>%
    filter(STATION_CODE == "15B" & PARAM %in% c("AY", "AY380", "ABS380")) %>%
    xtabs(~MYEAR + PARAM, .)  
  
  # CHECK MEDIANS LAST YEAR
  # Contains PYR1OH  
  dat_110_old <- readRDS("Data/110_mediandata_updated_2022-09-23.rds")
  dat_110_old %>%
    filter(STATION_CODE == "15B" & grepl("pyr", PARAM, ignore.case = T)) %>%
    xtabs(~MYEAR + PARAM, .)  
  dat_110_old %>%
    filter(STATION_CODE == "15B" & PARAM %in% "PYR1OH" & Basis == "WW" & MYEAR >= 2001) %>% 
    ggplot(aes(MYEAR, Value)) +
    geom_smooth( )+
    geom_point()


}


