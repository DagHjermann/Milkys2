
#
# Get data from targets objects and save in this folder
#

## Packages  

library(targets)
library(purrr)
library(dplyr)
library(ggplot2)

source("../Milkys3/R/001_functions_trend.R")

# Key variables (defines one time series) 
indexvars <- c("PARAM", "STATION_CODE", "TISSUE_NAME", "LATIN_NAME", "Basis")


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
## Get targets data ---- 
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# Set where to find the targets data  
targets::tar_config_set(store = "../Milkys3/_targets")
targets::tar_config_get("store")
# test <- tar_read(series3_bro.fish1)

# . The raw data ----

head(tar_read(data5_bro.fish1), 3)
head(tar_read(data5_bro.fish1), 3)
head(tar_read(data5_bro.fish1), 3)

data2 <- list(
  tar_read(data2_bro.fish1),
  tar_read(data2_bro.fish2),
  tar_read(data2_bro.fish3),
  tar_read(data2_bro.muss),
  tar_read(data2_met.fish1),
  tar_read(data2_met.fish2),
  tar_read(data2_met.fish3),
  tar_read(data2_met.muss),
  tar_read(data2_other),
  tar_read(data2_PAH.muss),
  tar_read(data2_PCB.fish1),
  tar_read(data2_PCB.fish2),
  tar_read(data2_PCB.fish3),
  tar_read(data2_PCB.muss),
  tar_read(data2_PFA.fish1),
  tar_read(data2_PFA.fish2),
  tar_read(data2_rem1.fish1),
  tar_read(data2_rem1.fish2),
  tar_read(data2_rem1.fish3),
  tar_read(data2_rem1.muss),
  tar_read(data2_rem2.fish1),
  tar_read(data2_rem2.fish3),
  tar_read(data2_rem2.muss),
  tar_read(data2_rem2.snai),
  tar_read(data2_HGa.fish)      # Hg length-adjusted (basis = WWa) - MUST BE THE LAST ONE
)


data5 <- list(
  tar_read(data5_bro.fish1),
  tar_read(data5_bro.fish2),
  tar_read(data5_bro.fish3),
  tar_read(data5_bro.muss),
  tar_read(data5_met.fish1),
  tar_read(data5_met.fish2),
  tar_read(data5_met.fish3),
  tar_read(data5_met.muss),
  tar_read(data5_other),
  tar_read(data5_PAH.muss),
  tar_read(data5_PCB.fish1),
  tar_read(data5_PCB.fish2),
  tar_read(data5_PCB.fish3),
  tar_read(data5_PCB.muss),
  tar_read(data5_PFA.fish1),
  tar_read(data5_PFA.fish2),
  tar_read(data5_rem1.fish1),
  tar_read(data5_rem1.fish2),
  tar_read(data5_rem1.fish3),
  tar_read(data5_rem1.muss),
  tar_read(data5_rem2.fish1),
  tar_read(data5_rem2.fish3),
  tar_read(data5_rem2.muss),
  tar_read(data5_rem2.snai),
  tar_read(data5_HGa.fish)      # Hg length-adjusted (basis = WWa) - MUST BE THE LAST ONE
)

# . The time series ----   
series3 <- list(
  tar_read(series3_bro.fish1),
  tar_read(series3_bro.fish2),
  tar_read(series3_bro.fish3),
  tar_read(series3_bro.muss),
  tar_read(series3_met.fish1),
  tar_read(series3_met.fish2),
  tar_read(series3_met.fish3),
  tar_read(series3_met.muss),
  tar_read(series3_other),
  tar_read(series3_PAH.muss),
  tar_read(series3_PCB.fish1),
  tar_read(series3_PCB.fish2),
  tar_read(series3_PCB.fish3),
  tar_read(series3_PCB.muss),
  tar_read(series3_PFA.fish1),
  tar_read(series3_PFA.fish2),
  tar_read(series3_rem1.fish1),
  tar_read(series3_rem1.fish2),
  tar_read(series3_rem1.fish3),
  tar_read(series3_rem1.muss),
  tar_read(series3_rem2.fish1),
  tar_read(series3_rem2.fish3),
  tar_read(series3_rem2.muss),
  tar_read(series3_rem2.snai),
  tar_read(series3_HGa.fish)      # Hg length-adjusted (basis = WWa) - MUST BE THE LAST ONE
)

# . The trend analysis results ----
result_list_object <- list(
  tar_read(result_bro.fish1),
  tar_read(result_bro.fish2),
  tar_read(result_bro.fish3),
  tar_read(result_bro.muss),
  tar_read(result_met.fish1),
  tar_read(result_met.fish2),
  tar_read(result_met.fish3),
  tar_read(result_met.muss),
  tar_read(result_other),
  tar_read(result_PAH.muss),
  tar_read(result_PCB.fish1),
  tar_read(result_PCB.fish2),
  tar_read(result_PCB.fish3),
  tar_read(result_PCB.muss),
  tar_read(result_PFA.fish1),
  tar_read(result_PFA.fish2),
  tar_read(result_rem1.fish1),
  tar_read(result_rem1.fish2),
  tar_read(result_rem1.fish3),
  tar_read(result_rem1.muss),
  tar_read(result_rem2.fish1),
  tar_read(result_rem2.fish3),
  tar_read(result_rem2.muss),
  tar_read(result_rem2.snai),
  tar_read(result_HGa.fish)     # Hg length-adjusted (basis = WWa)
)

# X <- tar_read(result_bro.fish1)[[1]]
# str(X, 1)
# str(X$result, 1)

### . Add WW (or WWa in one case) ----

#- Add "WW" to each of the series3 and result_list_object, except if it's
# the last one, then we add "WWa"  

N <- length(result_list_object)

# The following series are with WWa data, not WW:
k_WWa <- N

for (k in 1:N){
  
  if (k %in% k_WWa){
    # Hg length-adjusted (basis = WWa)
    basis <- "WWa"
  } else {
    basis <- "WW"
  }

  # Workaround for data2
  data2[[k]]$Basis <- basis
  
  # Workaround for data5
  data5[[k]]$Basis <- basis

  # Workaround for series3
  series3[[k]]$Basis <- basis
  
  # Workaround for result_list_object:
  for (i in 1:length(result_list_object[[k]])){
    if (!is.null(result_list_object[[k]][[i]]$result))
      result_list_object[[k]][[i]]$result$Basis <- basis
  }
  
}

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
## Make data frame, raw data ---- 
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

dat_raw2 <- data2 %>%
  map(~.x[c(indexvars, "MYEAR", "VALUE_WW", "VALUE_WWa", "FLAG1")]) %>%
  list_rbind() %>%
  mutate(
    VALUE = case_when(
      Basis == "WW" ~ VALUE_WW,
      Basis == "WWa" ~ VALUE_WWa)
  ) %>%
  select(-VALUE_WW, -VALUE_WWa)

## . Save ---- 

saveRDS(dat_raw2, "App01_timeseries/Data2022/dat_raw2.rds")


dat_raw5 <- data5 %>%
  map(~.x[c(indexvars, "x", "y", "VALUE", "Flag")]) %>%
  list_rbind()

## . Save ---- 

saveRDS(dat_raw5, "App01_timeseries/Data2022/dat_raw5.rds")

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
## Make data frame, one row per trend result ---- 
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


### . Get trend dataframe  ----  

dat_trend_list <- map2(series3, result_list_object, get_dftrend, last_year = 2022)

# Number of rows per series group
map_int(dat_trend_list, nrow)

dat_trend <- list_rbind(dat_trend_list)  

## . Save ---- 

saveRDS(dat_trend, "App01_timeseries/Data2022/dat_trend.rds")


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
## Extract plot data: trend lines ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


### Make data frame for all trend results   

# - One row per series   
# - Done by "flattening" the list of lists     

# Get all results, including failed ones  
result_list_all <- flatten(result_list_object)
length(result_list_all)

# Which time series did not fail?  
ok <- map(result_list_all, "result") %>% map_int(length) > 0

# Result objects with actual results  
result_list <- result_list_all[ok] %>% map("result")
length(result_list)

### . Save ---- 

saveRDS(result_list, "App01_timeseries/Data2022/result_list.rds")


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
## Test: get and plot trend line data ----    
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

### -- Series data frame
# Used only for getting the index i for selecting from results_list  

result_list_series <- seq_along(result_list) %>% 
  map(~as.data.frame(result_list[[.x]][indexvars])) %>%
  list_rbind()

### --- Select parameter and station  

sel_param <- "CB153" 
sel_station <- "98B1" 
sel_station <- "11X" 
sel_basis <- "WW" 

### -- Select series  

sel <- with(
  result_list_series,
  PARAM == sel_param & STATION_CODE == sel_station & Basis == sel_basis)
if (sum(sel) > 1){
  stop("More than one series series")
}
i <- which(sel) 

### -- Get trend line data    

indexvars <- c("PARAM", "STATION_CODE", "TISSUE_NAME", "LATIN_NAME", "Basis")
df1 <- as.data.frame(result_list[[i]][indexvars])
k_sel <- result_list[[i]]$k_sel
df2 <- result_list[[i]]$plot_data[[k_sel]]
result <- bind_cols(df1, df2)

### -- Get regression result strings      

sel2 <- with(
  dat_trend,
  PARAM == sel_param & STATION_CODE == sel_station & Basis == sel_basis)
if (sum(sel2) > 2){
  stop("More than two rows of results found (should be short and long trend)")
}
trendstring_short <- dat_trend[sel2 & dat_trend$Trend_type == "short",]$Trend_string
trendstring_long <- dat_trend[sel2 & dat_trend$Trend_type == "long",]$Trend_string

### --- Get raw data  

sel3a <- with(
  dat_raw2,
  PARAM == sel_param & STATION_CODE == sel_station & Basis == sel_basis & !is.na(VALUE))

sel3b <- with(
  dat_raw5,
  PARAM == sel_param & STATION_CODE == sel_station & Basis == sel_basis & !is.na(y))

# data (log-transformed)  
# dat_raw <- tar_read(data5_bro.fish1) %>% semi_join(df1)

### -- Test plot  

ggplot(result, aes(x = x)) +
  geom_point(data = dat_raw5[sel3b,], aes(y = y)) +
  geom_path(aes(y = y)) +
  geom_path(aes(y = y_q2.5), linetype = "dashed") +
  geom_path(aes(y = y_q97.5), linetype = "dashed") +
  labs(
    title = paste(sel_param, sel_station, sel_basis),
    subtitle = paste0("Long term: ", trendstring_long, ", short term:", trendstring_short)) 



# apply(is.na(dat_raw), 2, mean)
# data5[[1]] %>%
#   filter(is.na(y)) %>% View("na")
