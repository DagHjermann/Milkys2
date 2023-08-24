
#
# New run of particular ('pick') series 
#

# Run first part of 125 to get 'dat_all_prep3': 
#    - Run 0, 1 and 2a-2b 
#      (note that you might have to install 'leftcensored' in order to update to latest version)
#    - 2c and d1 may be skipped (d1 is slow), instead load data /dat_all_prep3) using the "commented out" line in 2.d2
#

# REMEMBER TO UPDATE folder_results!

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Make dat_series_pick - get key columns from data ----
#
# This is where we pick the data to re-analyse
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Use 'dat_rerun' from script 126 appendix B.
dat_rerun <- dat_rerun %>%
  distinct(PARAM, STATION_CODE, TISSUE_NAME, LATIN_NAME) %>%
  mutate(Pick = TRUE)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Make dat_series_pick - add columns needed for analysis ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


#
# Add the extra columns we need from the newly created 'dat_series' 
#

# version of max() that tolerates being given an x with length zero without warning
max_warningless <- function(x)
  ifelse(length(x)==0, NA, max(x))

dat_series_pick_1 <- dat_all_prep3 %>%
  # Select data using 'dat_rerun'
  left_join(dat_rerun) %>%
  filter(Pick) %>%
  # Also skip isotopes
  filter(!Substance.Group %in% "Isotopes") %>%
  group_by(Substance.Group, PARAM, STATION_CODE, TISSUE_NAME, LATIN_NAME, x) %>%
  summarise(
    N = n(),
    N_over_LOQ = sum(uncensored == 1),
    P_over_LOQ = N_over_LOQ/N,
    .groups = "drop") %>%
  group_by(Substance.Group, PARAM, STATION_CODE, TISSUE_NAME, LATIN_NAME) %>%
  summarise(
    First_year = min(x),
    Last_year = max(x),
    N_years = n(),
    N_years_10yr = length(unique(x[x >= (last_year-10)])),
    Years_over_LOQ = sum(N_over_LOQ > 0),
    Last_year_over_LOQ = max_warningless(x[N_over_LOQ > 0]),
    .groups = "drop") %>% 
  filter(Last_year >= last_year) %>%
  mutate(
    Trend_model = case_when(
      Years_over_LOQ <= 1 ~ "No model",
      Years_over_LOQ %in% 2 & N_years %in% 2 ~ "No model",
      Years_over_LOQ %in% 2:4 & N_years >= 3 ~ "Mean",
      Years_over_LOQ %in% 5:6 ~ "Linear",
      Years_over_LOQ %in% 7:9 ~ "Smooth, k_max=3",
      Years_over_LOQ %in% 10:14 ~ "Smooth, k_max=4",
      Years_over_LOQ >= 15 ~ "Smooth, k_max=5"),
    Trend_model = factor(
      Trend_model,
      levels = c("No model", "Mean", "Linear", 
                 "Smooth, k_max=3", "Smooth, k_max=4", "Smooth, k_max=5")),
    k_max = case_when(
      Trend_model %in% "No model" ~ as.numeric(NA),
      Trend_model %in% "Mean" ~ 1,
      Trend_model %in% "Linear" ~ 2,
      Trend_model %in% "Smooth, k_max=3" ~ 3,
      Trend_model %in% "Smooth, k_max=4" ~ 4,
      Trend_model %in% "Smooth, k_max=5" ~ 5)
  )

nrow(dat_series_pick_1)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Make dat_series_pick - add 'series_no' column ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#
# Get 'seriesno' of the result files that we will remake 
#

#
# Get 'dat_series_trend' used to make files in     
#
# dat_series_trend <- readRDS("Data/125_dat_series_trend.rds")

#
# Get key columns from the result files themselves 
#
folder_results <- paste0("Data/125_results_2021_04")
fns <- dir(folder_results, full.names = TRUE) %>% sort()
result_list <-lapply(fns, readRDS)
cat(length(fns), "files read \n")

# Extract 'seriesno' from file name (just for checking, see nex chunk)
series_no_filename <- substr(fns, nchar(folder_results) + 8, nchar(folder_results) + 11)
# Extract key columns 
series_no <- map_dbl(result_list, "seriesno")
PARAM <- map_chr(result_list, "PARAM")
STATION_CODE <- map_chr(result_list, "STATION_CODE")
TISSUE_NAME <- map_chr(result_list, "TISSUE_NAME")
LATIN_NAME <- map_chr(result_list, "LATIN_NAME")
dat_series_resultfiles <- data.frame(series_no, PARAM, STATION_CODE, TISSUE_NAME, LATIN_NAME)
# Check that 'fileinfo' is in same order as 'dat_success' - if so, they can simply be joined
check <- series_no == as.numeric(series_no_filename)
if (mean(check) < 1){
  stop("Series number from file name does not fit series number in the result file")
} else {
  cat("Series number from file name fits series number in the result file")
}

#
# Add 'seriesno' and file names to dat_series_pick
#
dat_series_pick <- dat_series_pick_1 %>%
  # Skip the "no model" series
  filter(Trend_model != "No model") %>%
  left_join(dat_series_resultfiles)

if (sum(is.na(dat_series_pick$series_no)) > 0){
  stop("Some series are not from existing result files. Make new numbers for those.")
}

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Raw data - Check uncertainty ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Check uncertainty - either NA or 0.3 
dat_all_prep3 %>%
  filter(PARAM %in% unique(dat_series_pick$PARAM)) %>%
  xtabs(~addNA(Uncertainty) + PARAM, .)

# Set to 0.3 for the rest (NILU data)
sel <- with(dat_all_prep3, PARAM %in% unique(dat_series_pick$PARAM) & is.na(Uncertainty))
sum(sel)
dat_all_prep3$Uncertainty[sel] <- 0.3


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Run analyses ----
# 
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# if (TRUE){
  
  #
  # Do only once: registering cores
  #
  
  n_cores <- future::availableCores()
  n_cores
  # 4 / 16 / 64 
  
  # If 4 cores 
  cl <- makeCluster(n_cores)
  registerDoParallel(cores = (n_cores))
  
  # If many cores 
  cl <- makeCluster(n_cores-1)
  registerDoParallel(cores = (n_cores-1))
  
# }


# Old version of 'get_splines_results_seriesno' used only row number of the data series file
# series_no <- 1:nrow(dat_series_trend)

# New version uses column 'series_no'  
series_no <- dat_series_pick$series_no
range(series_no)
length(series_no) # 1507

#
# . test run ----
#
if (FALSE){
  # TEST
  series_no[1]   # 1841
  # debugonce(get_splines_results_seriesno)
  # debugonce(lc_fixedsplines_tp)
  get_splines_results_seriesno(series_no[1], 
                               dat_all_prep3, dat_series_pick, foldername = folder_results,
                               raftery = TRUE)
}


#
# 
# . run computations ----
#
t0 <- Sys.time()
result <- foreach(i = series_no, 
                  .export = c("dat_all_prep3","dat_series_trend")) %dopar%
  get_splines_results_seriesno_s(i, 
                                 dat_all_prep3, dat_series_pick, foldername = folder_results,
                                 raftery = TRUE)
t1 <- Sys.time()
t1-t0
# 30 min for 100 on 16 cores  
# 4.8 hours for 1960 on 64 cores

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Check results ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Read
folder_results <- paste0("Data/125_results_2021_02")
fns_all <- dir(folder_results, full.names = TRUE) %>% sort()

# Select files by date
fns_info <- file.info(fns_all)
table(lubridate::date(fns_info$mtime))
sel <- lubridate::date(fns_info$mtime) == lubridate::ymd("2022-09-17")
fns <- fns_all[sel]

# Read files
result_list <-lapply(fns, readRDS)
cat(length(fns), "files read \n")

# Extract key columns 
series_no <- map_dbl(result_list, "seriesno")
PARAM <- map_chr(result_list, "PARAM")
STATION_CODE <- map_chr(result_list, "STATION_CODE")
TISSUE_NAME <- map_chr(result_list, "TISSUE_NAME")
LATIN_NAME <- map_chr(result_list, "LATIN_NAME")

# Degree of success
jags_finished <- map_lgl(result_list, ~!is.null(.x$k_values_ok))
ok <- map_lgl(result_list, ~!is.null(.x$DIC))

table(ok)  # all TRUE

