---
title: "201 Make csv for big excel file"
output: 
  html_document:
    keep_md: true
    toc: true
    toc_float: true
---

## 0. Check this first  


### a. Which kind of csv file do you want to make?     
  
The resulting file will be imported into Excel, so set set   
* decimal_comma = TRUE of you use European-style Excel (`,` for decimal numbers, columns separated by semicolon)   
* decimal_comma = FALSE of you use American-style Excel (`.` for decimal numbers, columns separated by comma)   

```r
decimal_comma <- TRUE
# used in part 7
```


### b. Last year  
Last year with observations  

```r
last_year <- 2019
```


## 1. Libraries + functions

```r
update.packages("rlang")

# install safejoin, if that has not already been done
if (!"safejoin" %in% installed.packages()){
  devtools::install_github(repo = "moodymudskipper/safejoin")
}

library(dplyr)        
library(tidyr)        # gather()
library(purrr)        # map_ functions
library(stringr)
library(lubridate)
library(openxlsx)
library(mgcv)
library(AICcmodavg)   # AICc()
library(safejoin)     # safe_left_join() - from https://github.com/moodymudskipper/safejoin   

source("002_Utility_functions.R")
source("201_Make_big_excel_file_functions.R")
source("201_Time_series_write_to_Excel_functions.R", encoding = "UTF-8")
```

## 2. Data  

### a. Annual medians  

```r
files <- list_files("Data", 
                    pattern = "110_mediandata_updated_[:date:]", 
                    extension = "rds")

cat("\n")
data_list <- read_rds_file(folder = "Data", 
                          files, 
                          filenumber = 1, 
                          get_date = TRUE,
                          time_since_modified = TRUE)

data_med2 <- data_list$data
file_date <- data_list$file_date
```

```
## There are 2 files with pattern '110_mediandata_updated_[:date:]' and extension 'rds' to choose from 
## 
## File '110_mediandata_updated_2020-08-05.rds' (file number 1) has been read 
##   This is the newest file. If you want to read an older file, put a different 'filenumber' 
## 
## Time since this file was modified: 
## Time difference of 1.63589 hours
```

Function for reading the last (by default) file made, of files with name following 'pattern'  

```r
list_and_read_rds_file <- function(folder, pattern,  
                                   filenumber = 1, check_date = NULL){
  
  date_pattern <- "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]"
  pattern_mod <- sub("[:date:]", date_pattern, pattern, fixed = TRUE)

  files <- dir(path = folder, 
               pattern = pattern_mod)
  cat("\n")
  data_list <- read_rds_file(folder = folder, 
                             files, 
                             filenumber = filenumber, 
                             get_date = TRUE,
                             time_since_modified = TRUE)
  result <- data_list$data
  if (!is.null(check_date)){
    if (data_list$file_date != check_date){
    stop("Date of ", sQuote(pattern), " is different from check_date!")
    }
  }
  
  result
}
```



### b. Time trend results   

```r
cat("======================================\n")
cat("10 year trends \n")
cat("======================================\n")

pattern_10yr_last <- paste0("^120_result_10yr_", last_year, "_[:date:].rds$")
pattern_10yr_seclast <- paste0("^120_result_10yr_", last_year-1, "_[:date:].rds$")

dir("Data", pattern = pattern_10yr_last)

cat("\n ******* Last year trends *********  \n")

# debugonce(list_and_read_rds_file)
result_10yr_last <- list_and_read_rds_file(
  folder = "Data", 
  pattern = pattern_10yr_last, 
  check_date = data_list$file_date
)

cat("\n ******* Previous year trends *********  \n")

result_10yr_seclast <- list_and_read_rds_file(
  folder = "Data", 
  pattern = pattern_10yr_seclast, 
  check_date = data_list$file_date
)

cat("\n")
cat("======================================\n")
cat("Long trends \n")
cat("======================================\n")

pattern_long_last <- paste0("^120_result_long_", last_year, "_[:date:].rds$")
pattern_long_seclast <- paste0("^120_result_long_", last_year-1, "_[:date:].rds$")

cat("\n ******* Last year trends *********  \n")

result_long_last <- list_and_read_rds_file(
  folder = "Data", 
  pattern = pattern_long_last, 
  check_date = data_list$file_date
)

cat("\n ******* Previous year trends *********  \n")

result_long_seclast <- list_and_read_rds_file(
  folder = "Data", 
  pattern = pattern_long_seclast, 
  check_date = data_list$file_date
)
```

```
## ======================================
## 10 year trends 
## ======================================
## character(0)
## 
##  ******* Last year trends *********  
## 
## File '120_result_10yr_2019_2020-08-05.rds' (file number 1) has been read 
## 
## Time since this file was modified: 
## Time difference of 1.669147 mins
## 
##  ******* Previous year trends *********  
## 
## File '120_result_10yr_2018_2020-08-05.rds' (file number 1) has been read 
## 
## Time since this file was modified: 
## Time difference of 14.76122 hours
## 
## ======================================
## Long trends 
## ======================================
## 
##  ******* Last year trends *********  
## 
## File '120_result_long_2019_2020-08-05.rds' (file number 1) has been read 
## 
## Time since this file was modified: 
## Time difference of 1.668996 mins
## 
##  ******* Previous year trends *********  
## 
## File '120_result_long_2018_2020-08-05.rds' (file number 1) has been read 
## 
## Time since this file was modified: 
## Time difference of 14.7612 hours
```

### c. Labware data  
Data of samples   
   
NOTE: These data are downloaded from Nivabasen using the script   
105_Download_Labware_sample_data.Rmd  
This must be done using a PC - cannot be done from Jupyterhub   

```r
# Read and reformat the most recent data (by default)  
files <- dir("Input_data", "Labware_samples_") %>% rev()
filename <- files[1] 

df_samples_labware_raw <- readRDS(paste0("Input_data/", filename))  
```


```r
# For test
data_med2 %>%
  filter(STATION_CODE == "36A" & PARAM == "% C" & Basis == "WW")
```

```
## # A tibble: 5 x 17
##   MYEAR STATION_CODE LATIN_NAME TISSUE_NAME PARAM UNIT  DRYWT FAT_PERC     N
##   <dbl> <chr>        <chr>      <chr>       <chr> <chr> <dbl>    <dbl> <int>
## 1  2015 36A          Mytilus e… Whole soft… % C   %        20     2.7      5
## 2  2016 36A          Mytilus e… Whole soft… % C   %        14     1.3      3
## 3  2017 36A          Mytilus e… Whole soft… % C   %        18     4.13     3
## 4  2018 36A          Mytilus e… Whole soft… % C   %        14     0.86     3
## 5  2019 36A          Mytilus e… Whole soft… % C   %        18     1.46     3
## # … with 8 more variables: Det_limit <dbl>, Over_LOQ <int>, Basis <chr>,
## #   Value <dbl>, Stations <chr>, N_stations <dbl>, Median <dbl>, Q95 <dbl>
```

### d. N-string, SD and DDI

```r
cat("======================================\n")
cat("N string (sample size) \n")
cat("======================================\n")

dat_nstring <- list_and_read_rds_file(
  folder = "Data", 
  pattern = "^111_Nstring_updated_[:date:].rds$", 
  check_date = data_list$file_date
)

cat("======================================\n")
cat("SD (standard deviation) \n")
cat("======================================\n")

dat_sd <- list_and_read_rds_file(
  folder = "Data", 
  pattern = "^111_SD_updated_[:date:].rds$", 
  check_date = data_list$file_date
)


cat("======================================\n")
cat("D.d.i. (detectable data information) \n")
cat("======================================\n")

dat_ddi <- list_and_read_rds_file(
  folder = "Data", 
  pattern = "^111_DDI_updated_[:date:].rds$", 
  check_date = data_list$file_date
)
```

```
## ======================================
## N string (sample size) 
## ======================================
## 
## File '111_Nstring_updated_2020-08-05.rds' (file number 1) has been read 
## 
## Time since this file was modified: 
## Time difference of 20.81439 days
## ======================================
## SD (standard deviation) 
## ======================================
## 
## File '111_SD_updated_2020-08-05.rds' (file number 1) has been read 
## 
## Time since this file was modified: 
## Time difference of 20.81439 days
## ======================================
## D.d.i. (detectable data information) 
## ======================================
## 
## File '111_DDI_updated_2020-08-05.rds' (file number 1) has been read 
## 
## Time since this file was modified: 
## Time difference of 20.81439 days
```


### e1. Other data   
1. Trends for second last year   
2. Individual data for this year (for SD)   
3. Second last year's table   
4. List of station names    
5. Data for the extra columns for parameters and stations  

```r
#
# 1. Trends for second last year (2018)
#
result_10yr_prev <- readRDS("Input_data/120_result_10yr_2018.RData")
result_long_prev <- readRDS("Input_data/120_result_long_2018.RData")
cat("1. Trends for second last year - data read \n")
```

```
## 1. Trends for second last year - data read
```

```r
#
# 2. Individual data for this year (for "D.d.i.")
#

if (FALSE){
data_lastyear_ind <- readRDS(paste0("Data/101_data_updated_", file_date, ".rds"))   
cat("\n")
cat("2. Individual data for this year - data read \n")

# Fixing station 227G1 and 227G2 (shall count as 227G) 
sel <- data_lastyear_ind$STATION_CODE %in% c("227G1","227G2")
data_lastyear_ind$STATION_CODE[sel] <- "227G"
cat(sum(sel), "lines with 227G1 and 227G2 changed to 227G \n")

# Fixing station 36A and 36A1 (36A1 shall count as 36A) 
sel <- data_lastyear_ind$STATION_CODE %in% "36A1"
data_lastyear_ind$STATION_CODE[sel] <- "36A"
cat(sum(sel), "lines with 36A1 changed to 36A \n")

#
# Check uniqueness (for last year only)
#
df <- data_lastyear_ind %>%
  filter(MYEAR %in% seq(last_year - 1, last_year)) %>%
  group_by(STATION_CODE, LATIN_NAME, TISSUE_NAME, MYEAR, SAMPLE_NO2, UNIT, PARAM) %>%
  mutate(n = n()) %>%
  filter(n > 1)
if (nrow(df) > 0)  # should result in zero
  stop("ERROR: There seems to be duplicates in 'data_lastyear_ind' \n")

}


#
# 3. Second last year's table
#
# results_seclast_year <- read.csv2("Input_data/Data_xl_lessthans_ver12.csv", encoding = "UTF-8")
results_seclast_year <- readRDS("Input_data/Data_xl_lessthans_ver12.rds") %>%
  mutate(TISSUE_NAME = case_when(
    TISSUE_NAME %in% "Liver" ~ "Lever",
    TISSUE_NAME %in% "Muscle" ~ "Muskel",
    TISSUE_NAME %in% "Bile" ~ "Galle",
    TISSUE_NAME %in% "Blood" ~ "Blod",
    TRUE ~ TISSUE_NAME)
    ) %>%
  filter(!STATION_CODE %in% c("227G1", "36A"))  # HARD-CODED - these dont have data in 2018 
cat("\n")
```

```r
cat("3. Second last year's table - data read \n")
```

```
## 3. Second last year's table - data read
```

```r
# table(results_seclast_year$TISSUE_NAME)

# Fixing station 36A and 36A1 (36A1 shall count as 36A) 
sel <- results_seclast_year$STATION_CODE %in% "36A1"
results_seclast_year$STATION_CODE[sel] <- "36A"
cat(sum(sel), "lines with 36A1 changed to 36A \n")
```

```
## 309 lines with 36A1 changed to 36A
```

```r
#
# Check uniqueness (for last year only)
#
df <- results_seclast_year %>%
  group_by(STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM, Basis) %>%
  mutate(n = n()) %>%
  filter(n > 1)
if (nrow(df) > 0)  # should result in zero
  stop("ERROR: There seems to be duplicates in 'data_lastyear_ind' \n")

if (FALSE){
  results_seclast_year %>%
    select(PARAM, LATIN_NAME, TISSUE_NAME, STATION_CODE, Basis, 
         Yr_2018, N_string, SD_last) %>%
    View()
}

# names(df)
# PARAM", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE", "Basis


#
# 4. List of station names   
#
data_stations <- readxl::read_excel("Input_data/Kartbase_edit.xlsx")
cat("\n")
```

```r
cat("4. List of station names - data read \n")
```

```
## 4. List of station names - data read
```

```r
#

#
# 5. Data for the extra columns for parameters and stations
#
df_par <- read.csv("Input_data/Lookup for big excel - param.csv", sep = ";", dec = ",", stringsAsFactors = FALSE)
df_stationinfo <- readxl::read_excel("Input_data/Lookup for big excel - stations.xlsx")

cat("\n")
```

```r
cat("5. Data for the extra columns for parameters and stations - data read \n")
```

```
## 5. Data for the extra columns for parameters and stations - data read
```

```r
#
# 6. Data on sample size ("N_string") and SD for the last two years
#
dat_sd <- readRDS("Data/111_SD_updated_2020-08-05.rds") 
dat_nstring <- readRDS("Data/111_Nstring_updated_2020-08-05.rds") 
```

### e2. Station coordinates   
Makes 'data_coordinates', added after less-than columns (section 6)

```r
#
# Read coordinates used in ICES
# Will be used for all stations that are *not* in 'data_stations' 
#
data_ices_coordinates <- read.csv2("Input_data/StationDictionary_20191104_utf8_Norway.csv",
                                   stringsAsFactors = FALSE) %>%
  mutate(STATION_CODE = stringr::str_extract(Station_Name, "[^[[:blank:]]]+"),
         Station_name = stringr::str_extract(Station_Name, "(?<=\\s).*")  # not kower/capital "n"
         ) %>%
  group_by(STATION_CODE) %>%
  mutate(StartYear_max = max(StartYear, na.rm = TRUE)) %>%
  filter(StartYear == StartYear_max)

check <- data_ices_coordinates %>%
  count(STATION_CODE) %>%
  filter(n > 1) %>%
  nrow()

if (check > 0){
  cat("WARNING! Duplicate STATION_CODE in the station file! \n")
}

#
# Combine coordinates from ICES with coordinates from data_stations (kartbase.xlsx)
#
data_coordinates <- bind_rows(
  # coordinates from ICES:
  data_ices_coordinates %>%
    filter(!STATION_CODE %in% data_stations$STATION_CODE) %>%
    rename(Long = Lon) %>%
    select(STATION_CODE, Station_name, Long, Lat),
  # coordinates from kartbase.xlsx:
  data_stations
)

writexl::write_xlsx(data_coordinates, "Data/201_data_coordinates.xlsx")
```


### f. Some data fixing  

```r
### 1. Delete VDSI that are not Basis WW    
sel <- with(data_med2, PARAM %in% c("VDSI","VDSI/Intersex") & !Basis %in% "WW")
data_med2 <- data_med2[!sel,]
cat(sum(sel), "records of VDSI/Intersex deleted \n")
```

```
## 1130 records of VDSI/Intersex deleted
```

```r
# Check
# data_med2 %>% filter(PARAM %in% "VDSI" & MYEAR %in% 2015:2017 & STATION_CODE %in% c("227G","227G2"))

### 2. BAP unit
sel <- with(data_med2, PARAM %in% "BAP")
# data_med2[sel,] %>% count(UNIT)
data_med2$UNIT[sel] <- "ug/kg/ABS 380 nm"
cat(sum(sel), "units of BAP changed \n")
```

```
## 2718 units of BAP changed
```

```r
# 3. Units of isotopes  
sel1 <- data_med2$PARAM %in% c("Delta13C", "Delta15N")
# xtabs(~UNIT, data_med2[sel1,])
sel2 <- sel1 & !data_med2$UNIT %in% "‰"
data_med2$UNIT[sel2] <- "‰"
cat(sum(sel2), "units of Delta13C and Delta15N changed \n")
```

```
## 420 units of Delta13C and Delta15N changed
```

```r
# 4. Set all Tissues containing "Egg" to exactly "Egg"
sel <- grepl("Egg", data_med2$TISSUE_NAME)
data_med2$TISSUE_NAME[sel] <- "Egg"
cat(sum(sel), "tissue names containing 'Egg' changed to the exact word 'Egg' \n")
```

```
## 1836 tissue names containing 'Egg' changed to the exact word 'Egg'
```


## 3. Some checking   

### a. One problematic species, one problematic station, one problematic parameter

```r
cat("==================================\n")
cat("Eider duck\n")
cat("----------------------------------\n")
check <- data_med2 %>% filter(LATIN_NAME %in% "Somateria mollissima" & MYEAR %in% last_year & Basis %in% "WW") %>%
  group_by(TISSUE_NAME, PARAM) %>%
  summarise(N = n(), .groups = "drop") %>%
  group_by(TISSUE_NAME, N) %>%
  summarise(PARAM = paste(PARAM, collapse = ", "), .groups = "drop") %>%
  as.data.frame()
# check
for (i in 1:nrow(check)){
  cat(check[i,"TISSUE_NAME"], "\nNumber of medians: N =", check[i,"N"], ":\n")
  cat(check[i,"PARAM"], "\n\n")
}

cat("==================================\n")
cat("71G\n")
cat("----------------------------------\n")
check <- data_med2 %>% filter(STATION_CODE %in% "71G" & Basis %in% "WW") %>%
  group_by(LATIN_NAME, PARAM, MYEAR) %>%
  summarise(N = n(), .groups = "drop") %>%
  group_by(LATIN_NAME, PARAM, N) %>%
  summarise(MYEAR = paste(MYEAR, collapse = ", "), .groups = "drop") %>%
  group_by(LATIN_NAME, N, MYEAR) %>%
  summarise(PARAM = paste(PARAM, collapse = ", "), .groups = "drop") %>%
  as.data.frame()
# check
for (i in 1:nrow(check)){
  cat(check[i,"LATIN_NAME"], ", ", check[i,"PARAM"], "\nNumber of medians: N =", check[i,"N"], ":\n")
  cat(check[i,"MYEAR"], "\n\n")
}


cat("==================================\n")
cat("PYR1O\n")
cat("----------------------------------\n")
check <- data_med2 %>% filter(PARAM %in% "PYR1O" & MYEAR >= 2008 & Basis %in% "WW") %>%
  group_by(STATION_CODE, MYEAR) %>%
  summarise(N = n(), .groups = "drop") %>%
  group_by(STATION_CODE, N) %>%
  summarise(MYEAR = paste(MYEAR, collapse = ", "), .groups = "drop") %>%
  group_by(MYEAR, N) %>%
  summarise(STATION_CODE = paste(STATION_CODE, collapse = ", "), .groups = "drop") %>%
  as.data.frame()
# check
for (i in 1:nrow(check)){
  cat(check[i,"STATION_CODE"], "\nNumber of medians since 2008: N =", check[i,"N"], ":\n")
  cat(check[i,"MYEAR"], "\n\n")
}
```

```
## ==================================
## Eider duck
## ----------------------------------
## Blod 
## Number of medians: N = 1 :
## % C, % N, AG, AS, BDE100, BDE119, BDE126, BDE138, BDE153, BDE154, BDE156, BDE17, BDE183, BDE184, BDE191, BDE196, BDE197, BDE202, BDE206, BDE207, BDE209, BDE28, BDE47, BDE49, BDE66, BDE6S, BDE71, BDE77, BDE85, BDE99, BDESS, C/N, CB_S7, CB101, CB105, CB114, CB118, CB122, CB123, CB128, CB138, CB141, CB149, CB153, CB156, CB157, CB167, CB170, CB18, CB180, CB183, CB187, CB189, CB194, CB206, CB209, CB28, CB31, CB33, CB37, CB47, CB52, CB66, CB74, CB99, CD, CO, CR, CU, D4, D5, D6, Delta13C, Delta15N, Fett, HBCDA, HBCDB, HBCDD, HBCDG, HCB, HG, MCCP, NI, PB, PFAS, PFDcA, PFHpA, PFHxA, PFHxS, PFNA, PFOA, PFOS, PFOSA, PFUdA, SCCP, SN, TBA, ZN 
## 
## Egg 
## Number of medians: N = 1 :
## % C, % N, AG, AS, BDE100, BDE119, BDE126, BDE138, BDE153, BDE154, BDE156, BDE17, BDE183, BDE184, BDE191, BDE196, BDE197, BDE202, BDE206, BDE207, BDE209, BDE28, BDE47, BDE49, BDE66, BDE6S, BDE71, BDE77, BDE85, BDE99, BDESS, C/N, CB_S7, CB101, CB105, CB114, CB118, CB122, CB123, CB128, CB138, CB141, CB149, CB153, CB156, CB157, CB167, CB170, CB18, CB180, CB183, CB187, CB189, CB194, CB206, CB209, CB28, CB31, CB33, CB37, CB47, CB52, CB66, CB74, CB99, CD, CO, CR, CU, D4, D5, D6, Delta13C, Delta15N, Fett, HBCDA, HBCDB, HBCDD, HBCDG, HCB, HG, MCCP, NI, PB, PFAS, PFDcA, PFHpA, PFHxA, PFHxS, PFNA, PFOA, PFOS, PFOSA, PFUdA, SCCP, SN, TBA, ZN 
## 
## ==================================
## 71G
## ----------------------------------
## Littorina littorea ,  DBT, DOT, DRYWT%, MBT, MOT, TBT, TCHT, TPT 
## Number of medians: N = 1 :
## 2015, 2016, 2017, 2018, 2019 
## 
## Littorina littorea ,  TTBT 
## Number of medians: N = 1 :
## 2017, 2018 
## 
## Littorina littorea ,  DBT-Sn, DOT-Sn, MBT-Sn, MOT-Sn, TBTIN, TCHT-Sn, TPhT-Sn, TTBT-Sn 
## Number of medians: N = 1 :
## 2017, 2018, 2019 
## 
## Littorina littorea ,  TTBTIN 
## Number of medians: N = 1 :
## 2019 
## 
## N. lapillus / L. littorea ,  VDSI/Intersex 
## Number of medians: N = 1 :
## 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2009, 2016, 2017, 2018, 2019 
## 
## Nucella lapillus ,  RSPI 
## Number of medians: N = 1 :
## 2001, 2002, 2003, 2004 
## 
## Nucella lapillus ,  DRYWT%, TBT 
## Number of medians: N = 1 :
## 2001, 2002, 2003, 2004, 2006, 2007, 2009 
## 
## Nucella lapillus ,  HTMEA, IMPS, LNFPE, LNMPE 
## Number of medians: N = 1 :
## 2005, 2006, 2007, 2009 
## 
## Nucella lapillus ,  TBTIN 
## Number of medians: N = 1 :
## 2006, 2007, 2009 
## 
## Nucella lapillus ,  DOT, MONOKTYL, MOT, TCHT, TETRABUT, TRISYKLO, TTBT 
## Number of medians: N = 1 :
## 2009 
## 
## ==================================
## PYR1O
## ----------------------------------
## 30B 
## Number of medians since 2008: N = 1 :
## 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019 
## 
## 15B, 23B, 53B 
## Number of medians since 2008: N = 1 :
## 2008, 2009, 2010, 2011, 2013, 2014, 2015, 2016, 2017, 2018, 2019
```

## 4. Prepare for building excel data

### a. Make 'data_xlvalues'  


```r
# This data set is used only for making select_data
data_for_select_data <- data_med2 %>%
  select(STATION_CODE, LATIN_NAME, PARAM, Basis, UNIT, MYEAR, Value) %>%
  group_by(STATION_CODE, LATIN_NAME, PARAM, Basis, UNIT) %>%
  mutate(Present_last7year = max(MYEAR) >= (last_year-7)) %>% 
  ungroup()

data_for_select_data %>%
  count(!is.na(Value), Present_last7year, !is.na(UNIT))

select_data <- with(data_for_select_data, MYEAR >= 1980 & !is.na(Value) & Present_last7year & !is.na(UNIT))
cat("Number of medians, originally:", nrow(data_med2), "\n")
cat("Number of medians, selected:", sum(select_data), "\n")

data_xlvalues1 <- data_med2[select_data,]
data_xlvalues <- data_xlvalues1 %>%
  select(MYEAR:UNIT, Basis, Value) %>%
  tidyr::pivot_wider(names_from = "MYEAR", values_from = "Value") %>%
  as.data.frame()

cat("Number of medians, selected, in wide format:", nrow(data_xlvalues), "\n")
```

```
## # A tibble: 8 x 4
##   `!is.na(Value)` Present_last7year `!is.na(UNIT)`      n
##   <lgl>           <lgl>             <lgl>           <int>
## 1 FALSE           FALSE             FALSE            2044
## 2 FALSE           FALSE             TRUE            91128
## 3 FALSE           TRUE              FALSE            1843
## 4 FALSE           TRUE              TRUE           153318
## 5 TRUE            FALSE             FALSE             740
## 6 TRUE            FALSE             TRUE            77574
## 7 TRUE            TRUE              FALSE            1037
## 8 TRUE            TRUE              TRUE           197152
## Number of medians, originally: 524836 
## Number of medians, selected: 197152 
## Number of medians, selected, in wide format: 24346
```

### b. Make less-than columns (as TRUE/FALSE)  

```r
data_lessthans <- data_med2[select_data,] %>%
  mutate(Lessthan = Over_LOQ/N < 0.5) %>%
  select(MYEAR:UNIT, Basis, Lessthan) %>%
  # select(-STATION_NAME) %>%     # Some station names varies, e.g. 02B (Kirkøy nord or 'Kirkøy (north)')
  spread(MYEAR, Lessthan) %>%
  as.data.frame()

# Change column names 
# We now use the names Yr_ and EQS_ for medians and EQS columns, because they are easier to search for
# Note: before writing to Excel we will change these to V.. and Q..
cn <- colnames(data_lessthans)
isnum <- !is.na(as.numeric(cn))
```

```
## Warning: NAs introduced by coercion
```

```r
colnames(data_lessthans)[isnum] <- paste0("Lt_", cn[isnum])

# colnames(data_lessthans)
# str(data_lessthans)

if (nrow(data_xlvalues) != nrow(data_lessthans)){
  cat("\n")
  cat("WARNING! data_xlvalues and data_lessthans has different number of lines! \n")
  cat("Don't continue, the results will be wrong. \n")
} else {
  cat("\n")
  cat("data_lessthans created and appears to be ok \n")
}

# Check (all must be equal, ie all numbers below should be zero!)

cat("\n")
cat("Check of index columns - all the following should be zero: \n")
sum(data_xlvalues$PARAM != data_lessthans$PARAM)
sum(data_xlvalues$STATION_CODE != data_lessthans$STATION_CODE)
sum(data_xlvalues$LATIN_NAME != data_lessthans$LATIN_NAME)
sum(data_xlvalues$TISSUE_NAME != data_lessthans$TISSUE_NAME)
sum(data_xlvalues$Basis != data_lessthans$Basis)

# head(data_lessthans,1)
```

```
## 
## data_lessthans created and appears to be ok 
## 
## Check of index columns - all the following should be zero: 
## [1] 24195
## [1] 24264
## [1] 14020
## [1] 13588
## [1] 17223
```

### c. Check that we don't have any duplicates  

```r
checkdata <- data_xlvalues %>%
  group_by(STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM, Basis) %>%
  summarise(N = n(), .groups = "drop_last") %>%
  filter(N > 1) 
cat("Number of data with duplicates:", nrow(checkdata), "\n")
```

```
## Number of data with duplicates: 0
```

```r
# The stuff below is run only if we have duplicates:
if (nrow(checkdata) > 0){
  i <- 1  # check duplicate number i
  df1 <- checkdata[i,] %>% 
    mutate(Key = paste(STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM, Basis, sep = "_"))
  df2 <- data_xlvalues %>% 
    mutate(Key = paste(STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM, Basis, sep = "_"))
  sel <- df2$Key %in% df1$Key; sum(sel)
  data_xlvalues[sel,]
  
  # 02B    Kirkøy nord 
  # 02B Kirkøy (north)
  
  # Check stations last year
  results_seclast_year %>%
    filter(STATION_CODE %in% "02B") %>%
    count(STATION_CODE, Station.Name)
}
```

### d. Change column names

```r
# We now use the names Yr_ and EQS_ for medians and EQS columns, because they are easier to search for
# Note: before writing to Excel we will change these to V.. and Q..
cn <- colnames(data_xlvalues)
isnum <- !is.na(as.numeric(cn))
```

```
## Warning: NAs introduced by coercion
```

```r
# colnames(data_xlvalues)[isnum] <- paste0("Yr_", substr(cn[isnum], 3, 4))
colnames(data_xlvalues)[isnum] <- paste0("Yr_", cn[isnum])

cat("\n")
```

```r
cat("Column names: \n")
```

```
## Column names:
```

```r
colnames(data_xlvalues)
```

```
##  [1] "STATION_CODE" "LATIN_NAME"   "TISSUE_NAME"  "PARAM"        "UNIT"        
##  [6] "Basis"        "Yr_1981"      "Yr_1982"      "Yr_1983"      "Yr_1984"     
## [11] "Yr_1985"      "Yr_1986"      "Yr_1987"      "Yr_1988"      "Yr_1989"     
## [16] "Yr_1990"      "Yr_1991"      "Yr_1992"      "Yr_1993"      "Yr_1994"     
## [21] "Yr_1995"      "Yr_1996"      "Yr_1997"      "Yr_1998"      "Yr_1999"     
## [26] "Yr_2000"      "Yr_2001"      "Yr_2002"      "Yr_2003"      "Yr_2004"     
## [31] "Yr_2005"      "Yr_2006"      "Yr_2007"      "Yr_2008"      "Yr_2009"     
## [36] "Yr_2010"      "Yr_2011"      "Yr_2012"      "Yr_2013"      "Yr_2014"     
## [41] "Yr_2015"      "Yr_2016"      "Yr_2017"      "Yr_2018"      "Yr_2019"
```

### e. Add EQS limits  

```r
EQS_limits <- read.xlsx("Input_data/EQS_limits.xlsx", "EQS")[,1:8] %>%
  filter(!is.na(PARAM))
EQS_limits <- fact2char_df(EQS_limits)  # changes all factor variables to character variables
EQS_limits$Limit <- as.numeric(EQS_limits[["Grense.brukt"]])

# Make table based on 'data_xlvalues'
data_EQS <- data_xlvalues
colnames(data_EQS) <- sub("Yr_", "EQS_", colnames(data_EQS), fixed = TRUE)
# head(data_EQS, 2)

# Delete all data values
sel_cols <- grepl("EQS_", colnames(data_EQS))
data_EQS[,sel_cols] <- NA
colno_values <- which(sel_cols)

# Add limit
data_EQS <- safe_left_join(
  data_EQS, 
  EQS_limits[,c("PARAM","Limit")] %>% filter(!is.na(PARAM)), 
  by = "PARAM",
  na_matches = "never",
  check = "BCV")

# Preliminary check:
# cat("Number of data with/without and EQS limit (with = TRUE): \n ")
# table(!is.na(data_EQS$Limit))

# Remove limit where Basis isn'n WW or WWa
sel <- !data_EQS$Basis %in% c("WW","WWa")
# mean(sel)
data_EQS$Limit[sel] <- NA

cat("Number of data with/without and EQS limit (with = TRUE): \n ")
```

```
## Number of data with/without and EQS limit (with = TRUE): 
## 
```

```r
table(!is.na(data_EQS$Limit))
```

```
## 
## FALSE  TRUE 
## 23297  1049
```

```r
# Add "n" in rows with EQS and in the columns where 'data_xlvalues' has values
# Why 'n'? Because it can be changed into a circle in Excel (by changing font to WIngdigs/Webdings)

# Select rows with Limit
sel_rows <- which(!is.na(data_EQS$Limit))
# length(sel_rows)

# Pick the columns with values only ('data_xlvalues_vals')
data_EQS_vals <- data_EQS[,colno_values]
data_xlvalues_vals <- data_xlvalues[,colno_values]
for (i in sel_rows){
  data_EQS_vals[i, !is.na(data_xlvalues_vals[i,])] <- "n"
}
# Put the columns with values only back into data_EQS
data_EQS[,colno_values] <- data_EQS_vals

# Check that the columns fit together
cat("\n")
```

```r
cat("Check that the columns fit together (row one = data_xlvalues, row two = data_EQS): \n")
```

```
## Check that the columns fit together (row one = data_xlvalues, row two = data_EQS):
```

```r
i <- with(data_xlvalues, PARAM %in% "HG" & STATION_CODE %in% "10A2" & Basis == "WW")
i <- with(data_xlvalues, PARAM %in% "HG" & STATION_CODE %in% "30B" & Basis == "WW")
a <- data_xlvalues[i, ] %>% as.matrix(ncol = 1)
b <- data_EQS[i, 1:ncol(a)] %>% as.matrix(ncol = 1)
rbind(a, b)
```

```
##     STATION_CODE LATIN_NAME     TISSUE_NAME PARAM UNIT      Basis Yr_1981
## 291 "30B"        "Gadus morhua" "Muskel"    "HG"  "MG_P_KG" "WW"  NA     
## 291 "30B"        "Gadus morhua" "Muskel"    "HG"  "MG_P_KG" "WW"  NA     
##     Yr_1982 Yr_1983 Yr_1984 Yr_1985 Yr_1986    Yr_1987    Yr_1988   Yr_1989  
## 291 NA      NA      "0.14"  "0.09"  "0.073188" "0.038171" "0.11169" "0.13026"
## 291 NA      NA      "n"     "n"     "n"        "n"        "n"       "n"      
##     Yr_1990 Yr_1991 Yr_1992 Yr_1993 Yr_1994 Yr_1995 Yr_1996   Yr_1997  
## 291 "0.12"  "0.09"  "0.12"  "0.133" "0.123" "0.105" "0.14075" "0.16075"
## 291 "n"     "n"     "n"     "n"     "n"     "n"     "n"       "n"      
##     Yr_1998  Yr_1999 Yr_2000 Yr_2001 Yr_2002 Yr_2003 Yr_2004 Yr_2005 Yr_2006
## 291 "0.1931" "0.201" "0.217" "0.26"  "0.15"  "0.163" "0.134" "0.147" "0.225"
## 291 "n"      "n"     "n"     "n"     "n"     "n"     "n"     "n"     "n"    
##     Yr_2007 Yr_2008 Yr_2009 Yr_2010 Yr_2011 Yr_2012 Yr_2013 Yr_2014 Yr_2015
## 291 "0.17"  "0.255" "0.26"  "0.22"  "0.316" "0.34"  "0.318" "0.207" "0.227"
## 291 "n"     "n"     "n"     "n"     "n"     "n"     "n"     "n"     "n"    
##     Yr_2016 Yr_2017 Yr_2018 Yr_2019
## 291 "0.364" "0.203" "0.204" "0.19" 
## 291 "n"     "n"     "n"     "n"
```

## 5. Build data set 'data_xl', which will become the big excel file   

### Start making data_xl  

```r
data_xl <- data_xlvalues %>% 
  rename(Unit = UNIT) %>% select(STATION_CODE, TISSUE_NAME, LATIN_NAME, PARAM, Basis, Unit)

cat("Number of columns in data_xl:",  ncol(data_xl), "\n")
```

```
## Number of columns in data_xl: 6
```

```r
# should be 6
```

### Prepare 'df_stationinfo' (station information)  

```r
# Assure STATION_CODE has no empty values
data_stations <- data_stations %>%
  filter(!is.na(STATION_CODE))

# Add new names
df_stationinfo$Station.Name <- NULL
df_stationinfo <- safe_left_join(
  df_stationinfo, 
  data_stations %>% select("STATION_CODE","Station_name"), 
  by = "STATION_CODE",
  na_matches = "never",
  check = "BCV"
  )

# Correct column order
# dput(colnames(df_stationinfo))
cols <- c("STATION_CODE", "Station_name", "Area", "County", "Water.Region", "VannforekomstID", "VAnnforekomstNavn")
df_stationinfo <- df_stationinfo[,cols]
colnames(df_stationinfo)[2] <- "Station.Name"
```

### Prepare parameter columns

```r
colnames(df_par)[1] <- "PARAM"

# Some IUPAC values contain semicolon, which makes a mess in Excel (as we use a semicolon-separated file)
# We replace the semicolon by a slash
# View(df_par)
sel <- grepl(";", df_par$IUPAC, fixed = TRUE)
cat("Change", sum(sel), "IPUAC values by replacing semicolon with slash \n")

cat("\n")
cat("Old names: \n")
df_par$IUPAC[sel]
df_par$IUPAC <- gsub(";", " / ", df_par$IUPAC, fixed = TRUE)
cat("\n")
cat("New names: \n")
df_par$IUPAC[sel]

# Change sum parameter names som they fit with the our "new" names
# Also see section 28
sel <- df_par$Parameter.Code %in% "PK_S"
if (sum(sel) > 0)
  df_par$Parameter.Code[sel] <- "KPAH"
cat(sum(sel), "cases: PK_S changed to KPAH \n")

sel <- df_par$Parameter.Code %in% "PAHSS"
if (sum(sel) > 0)
  df_par$Parameter.Code[sel] <- "PAH16"
cat(sum(sel), "cases: PAHSS changed to PAH16 \n")
```

```
## Change 3 IPUAC values by replacing semicolon with slash 
## 
## Old names: 
## [1] "potassium;1,1,2,2,3,3,4,4,4-nonafluorobutane-1-sulfonate"                             
## [2] "azanium;1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,10-henicosafluorodecane-1-sulfonate"
## [3] "azanium;1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,10-henicosafluorodecane-1-sulfonate"
## 
## New names: 
## [1] "potassium / 1,1,2,2,3,3,4,4,4-nonafluorobutane-1-sulfonate"                             
## [2] "azanium / 1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,10-henicosafluorodecane-1-sulfonate"
## [3] "azanium / 1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,10-henicosafluorodecane-1-sulfonate"
## 0 cases: PK_S changed to KPAH 
## 0 cases: PAHSS changed to PAH16
```


### Add new columns to data_xl

```r
# Add extra columns
data_xl <- data_xl %>%
  safe_left_join(
  df_par  %>% filter(!is.na(PARAM)), 
  by = "PARAM",
  na_matches = "never",
  check = "BCV"
) %>%
  safe_left_join(
    df_stationinfo %>% filter(!is.na(STATION_CODE)), 
    by = "STATION_CODE",
    na_matches = "never",
    check = "BCV"
  )

cat("Number of columns in data_xl:", ncol(data_xl), "\n") # 17
```

```
## Number of columns in data_xl: 17
```

### Put columns in correct sequence  

```r
cols_sequence <- c("PARAM", "Parameter.Name", "IUPAC", "CAS", "Component.Name", "Substance.Group", "Unit",
                   "STATION_CODE", "Station.Name", "Area", "County", 
                   "Water.Region", "VannforekomstID", "VAnnforekomstNavn",
                   "LATIN_NAME", "TISSUE_NAME",  "Basis")
data_xl <- data_xl[,cols_sequence]

# If error, try:
# cols_sequence[!cols_sequence %in% colnames(data_xl)]
```

### Add some station info manually

```r
cat("\nNumber of cases changed (se code for details): \n") # 17
```

```
## 
## Number of cases changed (se code for details):
```

```r
sel <- data_xl$STATION_CODE %in% "19B"; sum(sel)
```

```
## [1] 305
```

```r
data_xl$Station.Name[sel] <- "Svalbard"
data_xl$County[sel] <- "Svalbard"

sel <- data_xl$STATION_CODE %in% "19N"; sum(sel)
```

```
## [1] 464
```

```r
data_xl$Station.Name[sel] <- "Breøyane"
data_xl$County[sel] <- "Svalbard"

sel <- data_xl$STATION_CODE %in% "I964"; sum(sel)
```

```
## [1] 213
```

```r
data_xl$Station.Name[sel] <- "Toraneskaien"
data_xl$County[sel] <- "Nordland"

sel <- data_xl$STATION_CODE %in% "227G2"; sum(sel)
```

```
## [1] 0
```

```r
data_xl$Station.Name[sel] <- "Flatskjær"
data_xl$County[sel] <- "Rogaland"

sel <- data_xl$STATION_CODE %in% "76A2"; sum(sel)
```

```
## [1] 201
```

```r
data_xl$Station.Name[sel] <- "Risøy"
data_xl$County[sel] <- "Aust-Agder"

sel <- data_xl$STATION_CODE %in% "97A3"; sum(sel)
```

```
## [1] 246
```

```r
data_xl$Station.Name[sel] <- "Bodø harbour"
data_xl$County[sel] <- "Nordland"

sel <- data_xl$STATION_CODE %in% "28A2"; sum(sel)
```

```
## [1] 279
```

```r
data_xl$Station.Name[sel] <- "Ålesund harbour"
data_xl$County[sel] <- "Møre og Romsdal"
data_xl$Water.Region[sel] <- "Møre og Romsdal"
data_xl$VAnnforekomstNavn[sel] <- "Borgundfjorden-vest"

sel <- data_xl$STATION_CODE %in% "I911"; sum(sel)
```

```
## [1] 90
```

```r
data_xl$Station.Name[sel] <- "Horvika"
data_xl$County[sel] <- "Møre og Romsdal"

sel <- data_xl$STATION_CODE %in% "I914"; sum(sel)
```

```
## [1] 180
```

```r
data_xl$Station.Name[sel] <- "Flåøya (southeast)"
data_xl$County[sel] <- "Møre og Romsdal"

sel <- data_xl$STATION_CODE %in% "I132"; sum(sel)
```

```
## [1] 297
```

```r
data_xl$Station.Name[sel] <- "Svensholmen"
data_xl$County[sel] <- "Vest-Agder"
data_xl$Water.Region[sel] <- "Agder"
data_xl$VAnnforekomstNavn[sel] <- "Kristiansandsfjorden-indre"
```

### Add PROREF (background values) and add median values    
Q95 = Proref  

```r
df_background <- data_med2 %>%
  group_by(PARAM, LATIN_NAME, TISSUE_NAME, Basis) %>%
  summarise_at(c("Stations", "N_stations", "N", "Median", "Q95"), first) %>%
  ungroup()

# Used in report - copy to Norman's K (K:\Avdeling\Mar\NOG\JMG\2018\Tabeller)
openxlsx::write.xlsx(df_background, "Data/201_Proref.xlsx")
```

### Adding "Stations", "N_stations", "N", "Median", "Q95"  

```r
# head(df_background , 3)
# dput(colnames(df_background))

data_xl <- safe_left_join(
  data_xl, 
  df_background, 
  by = c("PARAM", "LATIN_NAME", "TISSUE_NAME", "Basis"),
  na_matches = "never",
  check = "BCV")

# Change some column names
colnames(data_xl)[colnames(data_xl) %in% "Stations"] <- "Backgr_stations"
colnames(data_xl)[colnames(data_xl) %in% "N_stations"] <- "Backgr_Nstat"
colnames(data_xl)[colnames(data_xl) %in% "N"] <- "Backgr_N"

cat("Number of rows in data_xl:", nrow(data_xl), "\n") # 24661
```

```
## Number of rows in data_xl: 24346
```

```r
cat("Number of columns in data_xl:", ncol(data_xl), "\n") # 22
```

```
## Number of columns in data_xl: 22
```

```r
# head(data_xl, 2)
```

### Adding values from 'data_xlvalues' and EQS sign from 'data_EQS'  

```r
### Prepare for adding values from 'data_xlvalues' and EQS sign from 'data_EQS'  
ind_cols1 <- which(grepl("Yr_", colnames(data_xlvalues)))
ind_cols2 <- which(grepl("EQS_", colnames(data_EQS)))
length(ind_cols1) == length(ind_cols2)  # should be TRUE
```

```
## [1] TRUE
```

```r
# Pick every second column from 'data_xlvalues' and 'data_EQS'
for (i in 1:length(ind_cols1)){
  i1 <- ind_cols1[i]
  i2 <- ind_cols2[i]
  data_xl <- cbind(data_xl, data_xlvalues[,i1], data_EQS[,i2], stringsAsFactors = FALSE)
  colnames(data_xl)[ncol(data_xl) - 1] <- colnames(data_xlvalues)[i1]
  colnames(data_xl)[ncol(data_xl)] <- colnames(data_EQS)[i2]
}

# Add 1980 (no data but it is in the Excel file we mimic)
i <- which(colnames(data_xl) == "Yr_1981")
n <- ncol(data_xl)
data_xl <- data.frame(data_xl[,1:(i-1)], Yr_1980 = NA, EQS_1980 = NA, data_xl[,i:n])

# colnames(data_xl)
# str(data_xl)

cat("\nNumber of columns:", ncol(data_xl), "\n") # 102
```

```
## 
## Number of columns: 102
```

### N string etc. for second last year    
5 columns:   
* Ant.prøver.2018 (N string)  
* SD_2018  
* Klasse.2018
* EQSclass_2018
* EQSthreshold_2018  

#### N string (sample size string) and SD     

```r
# data_xl_b <- data_xl  # backup
# data_xl <- data_xl_b  # restore

data_xl <- safe_left_join(
  data_xl, 
  dat_nstring %>% 
    filter(MYEAR == last_year - 1) %>% 
    select(-c(MYEAR, N, N_pooled, Max_ind)), 
  by = c("PARAM", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE"),
  na_matches = "never",
  check = "BCV")
# Change variable name
names(data_xl)[ncol(data_xl)] <- paste0("Ant.prøver.", last_year - 1)

cat("\nNumber of columns:", ncol(data_xl), "\n") # 103
```

```
## 
## Number of columns: 103
```

```r
data_xl <- safe_left_join(
  data_xl, 
  dat_sd %>% 
    filter(MYEAR == last_year - 1) %>% 
    ungroup() %>%
    select(-MYEAR), 
  by = c("PARAM", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE", "Basis"),
  na_matches = "never",
  check = "BCV")
# Change variable name
names(data_xl)[ncol(data_xl)] <- paste0("SD_", last_year - 1)

cat("\nNumber of columns:", ncol(data_xl), "\n") # 104
```

```
## 
## Number of columns: 104
```

#### Add Klasse (proref class) previous year  

```r
colnumber_value <- which(colnames(data_xl) == paste0("Yr_", last_year-1))  # Find column number for that year's median
value_prevyear <- data_xl[,colnumber_value]

colnumber_lessthan <- which(colnames(data_lessthans) == paste0("Lt_", last_year-1))  # Find column number for less-than
lessthan_prevyear <- data_xl[,colnumber_value]

# Less-thans are set a tad lower, to be put in the lower class
sel <- !is.na(lessthan_prevyear) & lessthan_prevyear
value_prevyear[sel] <- value_prevyear[sel] - 0.00001

# Just to check that we get the correct classes, i.e., if the conc. is on the limit, we get the upper class (using right = FALSE)
check_classes <- cut(value_prevyear/data_lessthans$Q95, breaks = c(-999999,1,2,5,10,20,999999), right = FALSE)

cat("Classes for Value / Proref : \n")
```

```
## Classes for Value / Proref :
```

```r
levels(check_classes)
```

```
## [1] "[-1e+06,1)" "[1,2)"      "[2,5)"      "[5,10)"     "[10,20)"   
## [6] "[20,1e+06)"
```

```r
cat("\n")
```

```r
class_prevyear <- cut(value_prevyear/data_xl$Q95, breaks = c(-999999,1,2,5,10,20,999999), right = FALSE, labels = FALSE)
# str(class_prevyear)
# summary(class_prevyear)
# table(addNA(class_prevyear))

# Make variable
data_xl$Klasse.prevyear <- class_prevyear

# Tabulate
# table(addNA(data_xl$Klasse.lastyear))

# Set variable name
colnames(data_xl)[ncol(data_xl)] <- paste0("Klasse.", last_year - 1)

cat("\nNumber of rows:", nrow(data_xl), "\n\n") # 24346
```

```
## 
## Number of rows: 24346
```

```r
cat("\nNumber of columns:", ncol(data_xl), "\n") # 105
```

```
## 
## Number of columns: 105
```


#### Add EQSclass and EQS for last year   

```r
cols <- c("PARAM", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE", "Basis",
          paste0("EQSclass_", last_year - 1), 
          "EQS")

select_rows <- complete.cases(
  results_seclast_year[c("PARAM", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE", "Basis")])


# cols %in% colnames(results_seclast_year)
data_xl <- safe_left_join(
  data_xl, 
  results_seclast_year[select_rows, cols],
  by = c("PARAM", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE", "Basis"),
  na_matches = "never",
  check = "BCV"
  )

cat("\nNumber of columns:", ncol(data_xl), "\n") # 107
```

```
## 
## Number of columns: 107
```

```r
# Change column name
colnumber <- which(colnames(data_xl) == "EQS")
colnames(data_xl)[colnumber] <- paste0("EQSthreshold_", last_year - 1)
cat("Changed name for column number", colnumber, " \n")
```

```
## Changed name for column number 107
```


### N string and SD for last year    


```r
data_xl <- safe_left_join(
  data_xl, 
  dat_nstring %>% 
    filter(MYEAR == last_year) %>% 
    select(-c(MYEAR, N, N_pooled, Max_ind)), 
  by = c("PARAM", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE"),
  na_matches = "never",
  check = "BCV")

# Change variable name
names(data_xl)[ncol(data_xl)] <- "N_string"

cat(sum(!is.na(data_xl$N_string)), "values of 'N_string' added to data \n")
```

```
## 10842 values of 'N_string' added to data
```

```r
cat("Number of columns:", ncol(data_xl), "\n") # 108
```

```
## Number of columns: 108
```

```r
data_xl <- safe_left_join(
  data_xl, 
  dat_sd %>% 
    filter(MYEAR == last_year) %>% 
    ungroup() %>%
    select(-MYEAR), 
  by = c("PARAM", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE", "Basis"),
  na_matches = "never",
  check = "BCV")

# Change variable name
names(data_xl)[ncol(data_xl)] <- "SD_last"

cat("\n")
```

```r
cat(sum(!is.na(data_xl$SD_last)), "values of 'SD_last' added to data \n")
```

```
## 10375 values of 'SD_last' added to data
```

```r
cat("Number of columns:", ncol(data_xl), "\n") # 109
```

```
## Number of columns: 109
```



### Add Class for last year   
Class for second last year added below (section 27b)


```r
colnumber_value <- which(colnames(data_xl) == paste0("Yr_", last_year))  # Find column number for last year's median
value_lastyear <- data_xl[,colnumber_value]

colnumber_lessthan <- which(colnames(data_lessthans) == paste0("Lt_", last_year))  # Find column number for last year's less-than
lessthan_lastyear <- data_xl[,colnumber_value]

# Less-thans are set a tad lower, to be put in the lower class
sel <- !is.na(lessthan_lastyear) & lessthan_lastyear
value_lastyear[sel] <- value_lastyear[sel] - 0.00001

# Just to check that we get the correct classes, i.e., if the conc. is on the limit, we get the upper class (using right = FALSE)
check_classes <- cut(value_lastyear/data_lessthans$Q95, breaks = c(-999999,1,2,5,10,20,999999), right = FALSE)

cat("Classes for Value / Proref : \n")
```

```
## Classes for Value / Proref :
```

```r
levels(check_classes)
```

```
## [1] "[-1e+06,1)" "[1,2)"      "[2,5)"      "[5,10)"     "[10,20)"   
## [6] "[20,1e+06)"
```

```r
cat("\n")
```

```r
class_lastyear <- cut(value_lastyear/data_xl$Q95, breaks = c(-999999,1,2,5,10,20,999999), right = FALSE, labels = FALSE)
# str(class_lastyear)
# summary(class_lastyear)
# table(addNA(class_lastyear))

# Make variable
data_xl$Klasse.lastyear <- class_lastyear

# Tabulate
# table(addNA(data_xl$Klasse.lastyear))

# Set variable name
colnames(data_xl)[ncol(data_xl)] <- paste0("Klasse.", last_year)


cat("\nNumber of columns:", nrow(data_xl), "\n\n") # 110
```

```
## 
## Number of columns: 24346
```

```r
cat("\nNumber of columns:", ncol(data_xl), "\n") # 110
```

```
## 
## Number of columns: 110
```

### EQS class - put variable without data   


```r
# Now just give it a NA, then we set it after we have inserted the column for the EQS limit
# Then we will change the name, also 
data_xl$EQSclass_lastyear <- NA

cat("\nNumber of columns:", ncol(data_xl), "\n") # 111
```

```
## 
## Number of columns: 111
```

### Add EQS limit (WW and WWa only)   


```r
# Variable 'EQS'
# head(EQS_limits, 2)

EQS_limits <- EQS_limits %>%
  rename(EQS = Limit) %>%
  filter(!is.na(PARAM))  

cols <- c("PARAM", "EQS")
df <- rbind(
  data.frame(EQS_limits[,cols], Basis = "WW", stringsAsFactors = FALSE),
  data.frame(EQS_limits[,cols], Basis = "WWa", stringsAsFactors = FALSE)
)
# head(df)

data_xl <- safe_left_join(
  data_xl, 
  df, 
  by = c("PARAM", "Basis"), 
  na_matches = "never",
  check = "BCV")

# colnames(data_xl)
cat("\nNumber of columns:", ncol(data_xl), "\n") # 112
```

```
## 
## Number of columns: 112
```

### Set values of EQS class


```r
value_lastyear <- data_xl[[paste0("Yr_", last_year)]]
lessthan_lastyear <- data_lessthans[[paste0("Lt_", last_year)]]

# Less-thans are set a tad lower, to be put in the lower class
sel <- !is.na(lessthan_lastyear) & lessthan_lastyear; sum(sel)
```

```
## [1] 4362
```

```r
value_lastyear[sel] <- value_lastyear[sel] - 0.00001

# Fill variable with values
data_xl$EQSclass_lastyear <- cut(value_lastyear/data_xl$EQS, breaks = c(-999999,1,999999), right = FALSE, labels = FALSE)

# Change column name of last added variable
colnumber <- which(colnames(data_xl) %in% "EQSclass_lastyear")
colnames(data_xl)[colnumber] <- paste0("EQSclass_", last_year)

cat("\nNumber of columns:", ncol(data_xl), "\n") # still 112
```

```
## 
## Number of columns: 112
```

### Add OC  
No data  

```r
data_xl$OC <- NA
cat("\nNumber of columns:", ncol(data_xl), "\n") # 113
```

```
## 
## Number of columns: 113
```

### Trends for last year - preparations

```r
# Key columns:
cols <- c("PARAM", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE", "Basis")
# debugonce(set_symbol)

# Make data frames with trend symbols 
trend_long_for_excel <- make_trend_data_for_excel2(result_long_last, data_xl[,cols])
```

```
## 
## Attaching package: 'gtools'
```

```
## The following object is masked from 'package:mgcv':
## 
##     scat
```

```r
trend_10yr_for_excel <- make_trend_data_for_excel2(result_10yr_last, data_xl[,cols])

# head(trend_long_for_excel, 2)

# Combine log + 10yr and prepare even more for Excel
trends_for_excel <- combine_long_and_short_trends_for_excel2(trend_long_for_excel, trend_10yr_for_excel)

cat("'trends_for_excel', number of lines:", nrow(trends_for_excel), "\n\n")  # 29582

cat("'trends_for_excel', values: \n")  # 29582
table(trends_for_excel$Trend.year)

# Change column name
colnumber <- which(colnames(trends_for_excel) %in% "Trend.year")
colnames(trends_for_excel)[colnumber] <- paste0("Trends.", last_year)
# colnames(trends_for_excel)

check <- data_xl %>%
  group_by(STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM, Basis) %>%
  summarise(N = n())
```

```
## `summarise()` regrouping output by 'STATION_CODE', 'LATIN_NAME', 'TISSUE_NAME', 'PARAM' (override with `.groups` argument)
```

```r
cat("\n")
cat("Number of duplicates in data_xl (should be zero):", sum(check$N > 1), "\n")   

# If there are duplicates, check data:
if ( sum(check$N > 1)){
  check %>% filter(N > 1) %>% head(10)
  df1 <- check %>% filter(N > 1) %>% head(1)
  xtabs(~STATION_CODE + PARAM , check %>% filter(N > 1))
  check2 <- data_xl %>%
    filter(STATION_CODE %in% df1$STATION_CODE, LATIN_NAME %in% df1$LATIN_NAME,
           TISSUE_NAME %in% df1$TISSUE_NAME, PARAM %in% df1$PARAM, Basis %in% df1$Basis)
  check2
}

check <- trends_for_excel %>%
  group_by(STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM, Basis) %>%
  summarise(N = n())
```

```
## `summarise()` regrouping output by 'STATION_CODE', 'LATIN_NAME', 'TISSUE_NAME', 'PARAM' (override with `.groups` argument)
```

```r
cat("Number of duplicates in 'trends_for_excel' (should be zero):", sum(check$N > 1), "\n")   

# If there are duplicates, check data:
if ( sum(check$N > 1)){
  table(check$N)
  check %>% filter(N > 1) %>% head(10)
  df1 <- check %>% filter(N > 1) %>% head(1)
  check2 <- trends_for_excel %>% 
    filter(STATION_CODE %in% df1$STATION_CODE, LATIN_NAME %in% df1$LATIN_NAME, 
           TISSUE_NAME %in% df1$TISSUE_NAME, PARAM %in% df1$PARAM, Basis %in% df1$Basis)
  check2
}
```

```
## 'trends_for_excel', number of lines: 24346 
## 
## 'trends_for_excel', values: 
## 
##   «/«   «/§   «/¢   «/ê   §/§     /   ¢/«   ¢/§   ¢/¢   ¢/é   ¢/ê   ê/«   é/§ 
##  2388    24    19     3 16380     2    74    15  3112   111   145   108     1 
##   ê/§   é/¢   ê/¢   é/é   ê/é   ê/ê 
##     6    48   953   158    32   767 
## 
## Number of duplicates in data_xl (should be zero): 0 
## Number of duplicates in 'trends_for_excel' (should be zero): 0
```

### Trends for last year - add columns to data

```r
data_xl_b <- data_xl  # backup data
# data_xl <- data_xl_b  # restore from backup, if needed

data_xl <- safe_left_join(
  data_xl, 
  trends_for_excel, 
  by = cols, 
  na_matches = "never",
  check = "BCV")

cat("\nNumber of columns:", ncol(data_xl), "\n") # 124
```

```
## 
## Number of columns: 124
```

```r
# colnames(data_xl) 
```

### Trends from second last year - preparations  

```r
# Key columns:
cols <- c("PARAM", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE", "Basis")
# debugonce(set_symbol)

# Make data frames with trend symbols 
trend_long_for_excel_seclast <- make_trend_data_for_excel2(result_long_seclast, data_xl[,cols])
trend_10yr_for_excel_seclast <- make_trend_data_for_excel2(result_10yr_seclast, data_xl[,cols])

# head(trend_long_for_excel_seclast, 2)

# Combine log + 10yr and prepare even more for Excel
trends_for_excel_seclast <- combine_long_and_short_trends_for_excel2(trend_long_for_excel_seclast, 
                                                                     trend_10yr_for_excel_seclast)

cat("'trends_for_excel_seclast', number of lines:", nrow(trends_for_excel), "\n\n")  # 29582

cat("'trends_for_excel_seclast', values: \n")  # 29582
table(trends_for_excel_seclast$Trend.year)

# Existing names
var2 <- c("Trend p(long)", "Detectable % change(long)", "First Year(long)", "Last Year(long)",
          "No of Years(long)", "Trend p(short)", "Detectable % change(short)", "First Year(short)", "Last Year(short)",
          "No of Years(short)")
if (sum(colnames(trends_for_excel_seclast) %in% var2) != 10){
  message("Warning: one of the names doesn't fit (should be 10 names fitting)")
}

var2_new <- paste(var2, last_year-1)

# Set new names
for (i in 1:length(var2_new)){
  sel <- colnames(trends_for_excel_seclast) == var2[i]
  colnames(trends_for_excel_seclast)[sel] <- var2_new[i]
}

# Change last column name
colnumber <- which(colnames(trends_for_excel_seclast) %in% "Trend.year")
colnames(trends_for_excel_seclast)[colnumber] <- paste0("Trends.", last_year-1)
# colnames(trends_for_excel)


check <- trends_for_excel_seclast %>%
  group_by(STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM, Basis) %>%
  summarise(N = n())
```

```
## `summarise()` regrouping output by 'STATION_CODE', 'LATIN_NAME', 'TISSUE_NAME', 'PARAM' (override with `.groups` argument)
```

```r
cat("Number of duplicates in 'trends_for_excel_seclast' (should be zero):", sum(check$N > 1), "\n")   

# If there are duplicates, check data:
if ( sum(check$N > 1)){
  table(check$N)
  check %>% filter(N > 1) %>% head(10)
  df1 <- check %>% filter(N > 1) %>% head(1)
  check2 <- trends_for_excel_seclast %>% 
    filter(STATION_CODE %in% df1$STATION_CODE, LATIN_NAME %in% df1$LATIN_NAME, 
           TISSUE_NAME %in% df1$TISSUE_NAME, PARAM %in% df1$PARAM, Basis %in% df1$Basis)
  check2
}
```

```
## 'trends_for_excel_seclast', number of lines: 24346 
## 
## 'trends_for_excel_seclast', values: 
## 
##   «/«   «/§   «/¢   «/ê   §/§     /   ¢/«   ¢/§   ¢/¢   ¢/é   ¢/ê   ê/«   é/§ 
##  2388    24    19     3 16380     2    74    15  3112   111   145   108     1 
##   ê/§   é/¢   ê/¢   é/é   ê/é   ê/ê 
##     6    48   953   158    32   767 
## Number of duplicates in 'trends_for_excel_seclast' (should be zero): 0
```



### Trends from second last year - add columns to data


```r
data_xl_b <- data_xl    # backup
# data_xl <- data_xl_b  # revert to backup

data_xl <- safe_left_join(
  data_xl, 
  trends_for_excel_seclast, 
  by = cols,
  na_matches = "never",
  check = "BCV")

cat("\nNumber of columns:", ncol(data_xl), "\n") # 135
```

```
## 
## Number of columns: 135
```

### Add "Last_two_years" +  "DETLIM_..." for second last year  


```r
#
# Last_two_years
#
data_xl$Last_two_years <- NA

#
# DETLIM for second last year
#
cols <- c("PARAM", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE", "Basis", paste0("Det_limit_", last_year-1))

if ("Det_limit" %in% names(results_seclast_year)){
  sel <- names(results_seclast_year) == "Det_limit"
  names(results_seclast_year)[sel] <- paste0("Det_limit_", last_year-1)
}

# dput(colnames(results_seclast_year))
# results_seclast_year[1, cols[1:5]]

data_xl_b <- data_xl  # backup
# data_xl <- data_xl_b

data_xl <- safe_left_join(
  data_xl, 
  results_seclast_year[,cols], 
  by = c("PARAM", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE", "Basis"),
  na_matches = "never",
  check = "BCV")

cat("\nNumber of columns:", ncol(data_xl), "\n") # 137
```

```
## 
## Number of columns: 137
```

### Add "DETLIM_..." for last year  


```r
cols <- c("PARAM", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE", "Basis", "Det_limit")

# dput(colnames(results_seclast_year))
# results_seclast_year[1, cols[1:5]]

# check <- data_med2 %>%
#   group_by(PARAM, LATIN_NAME, TISSUE_NAME, STATION_CODE, Basis) %>%
#   summarize(N = n())
# table(check$N)
# check2 <- check %>% filter(N > 1) %>% head(1)
# data_med2 %>% filter(PARAM %in% check2$PARAM &
#                      LATIN_NAME %in% check2$LATIN_NAME & 
#                      TISSUE_NAME %in% check2$TISSUE_NAME &
#                      STATION_CODE %in% check2$STATION_CODE &
#                      Basis %in% check2$Basis)


data_xl_b <- data_xl  # backup
# data_xl <- data_xl_b

sel <- data_med2$MYEAR %in% last_year
data_xl <- safe_left_join(
  data_xl, 
  data_med2[sel,cols], 
  by = c("PARAM", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE", "Basis"),
  na_matches = "never",
  check = "BCV")

# Change name of last variable (add year)
sel <- length(names(data_xl))
names(data_xl)[sel] <- paste0("Det_limit_", last_year)

cat("\nNumber of columns:", ncol(data_xl), "\n") # 138
```

```
## 
## Number of columns: 138
```

### Add trend and EQS change  
4 columns: TREND_CHANGE	CLASS_CHANGE	EQS_CHANGE	EAC_CHANGE  

```r
# TREND_CHANGE
col_last <- paste0("Trends.", last_year)
col_seclast <- paste0("Trends.", last_year - 1)
sel <- !is.na(data_xl[,col_seclast]) & !is.na(data_xl[,col_last]) & data_xl[,col_seclast] != data_xl[,col_last]
data_xl$TREND_CHANGE <- NA
data_xl$TREND_CHANGE[sel] <- paste(data_xl[sel,col_seclast], "to", data_xl[sel,col_last])

cat("Number of changes in trend:", sum(sel), "\n")
```

```
## Number of changes in trend: 0
```

```r
# CLASS_CHANGE
col_last <- paste0("Klasse.", last_year)
col_seclast <- paste0("Klasse.", last_year - 1)
sel <- !is.na(data_xl[,col_seclast]) & !is.na(data_xl[,col_last]) & data_xl[,col_seclast] != data_xl[,col_last]
data_xl$CLASS_CHANGE <- NA
data_xl$CLASS_CHANGE[sel] <- paste(data_xl[sel,col_seclast], "to", data_xl[sel,col_last])

cat("Number of changes in class:", sum(sel), "\n")
```

```
## Number of changes in class: 1318
```

```r
# Check
# table(data_xl$CLASS_CHANGE)

# EQS classes second last year
value_seclast <- data_xl[,paste0("Yr_", last_year - 1)]
lessthan_seclast <- data_lessthans[,paste0("Lt_", last_year - 1)]
# Less-thans are set a tad lower, to be put in the lower class
sel <- !is.na(lessthan_seclast) & lessthan_seclast
value_seclast[sel] <- value_seclast[sel] - 0.00001
EQSclass_seclast <- cut(value_seclast/data_xl$EQS, breaks = c(-999999,1,999999), right = FALSE, labels = FALSE)

# EQS classes last year
EQSclass_last <- data_xl[,paste0("EQSclass_", last_year)]

# Set EQS_CHANGE
data_xl$EQS_CHANGE <- NA
sel <- !is.na(EQSclass_seclast) & !is.na(EQSclass_last) & 
        EQSclass_seclast != EQSclass_last  # pick all that are different
sum(sel)
```

```
## [1] 21
```

```r
data_xl$EQS_CHANGE[sel] <- paste(EQSclass_seclast[sel], "to", EQSclass_last[sel])

cat("Changes in EQS class: \n")
```

```
## Changes in EQS class:
```

```r
table(data_xl$EQS_CHANGE)
```

```
## 
## 1 to 2 2 to 1 
##     13      8
```

```r
# 1 to 2 2 to 1 
#      4     11

# EAC_CHANGE is not used
data_xl$EAC_CHANGE <- NA

cat("\nNumber of columns:", ncol(data_xl), "\n") # 142
```

```
## 
## Number of columns: 142
```


### D.d.i.- add to data

```r
cols <- c("PARAM", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE", "Basis", "DDI")

data_xl_b <- data_xl    # backup
# data_xl <- data_xl_b  # restore from backup

data_xl <- safe_left_join(
  data_xl, 
  dat_ddi[,cols], 
  by = c("PARAM", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE", "Basis"),
  na_matches = "never",
  check = "BCV")

cat("\nNumber of columns:", ncol(data_xl), "\n") # 143
```

```
## 
## Number of columns: 143
```

#### Check D.d.i.  

```r
if (FALSE){
  
  # colnames(data_xl)[seq(91,101,2)] %>% dput()
  
  vars <- c("PARAM", "STATION_CODE", "Basis", "Yr_2018", "Yr_2019", "DDI")
  
  data_xl[vars] %>%
    filter(STATION_CODE == "30B" & Basis == "WW")
  
  data_xl[vars] %>%
    filter(STATION_CODE == "36A" & PARAM == "% C" & Basis == "WW")
  
  data_xl[vars] %>%
    filter(is.na(DDI))
  
}
```


### Add trends as given 2016   
*Not* second last year - fixed to 2016     
- Using the old OSPAR rules for less-thans   

```r
var1 <- c("PARAM", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE", "Basis")

# Just for checking....
if (FALSE){
  var2 <- c("Trend.p.long.", "Detectable...change.long.", "First.Year.long.", "Last.Year.long.",
            "No.of.Years.long.", "Trend.p.short.", "Detectable...change.short.",
            "First.Year.short.", "Last.Year.short.", "No.of.Years.short.",
            "Trends.2016.old")
  
  var2 %in% colnames(results_seclast_year)
  which(colnames(results_seclast_year) %in% var2)
}

data_xl_b <- data_xl  # backup
# data_xl <- data_xl_b

data_xl <- safe_left_join(
  data_xl, 
  results_seclast_year[,c(var1, "Trends.2016.old")], 
  by = var1, 
  na_matches = "never",
  check = "BCV")

cat("\nNumber of columns:", ncol(data_xl), "\n") # 144
```

```
## 
## Number of columns: 144
```

### Add D.d.i. code  
This is an index number, but we can't add it yet since we need to keep the original row order for making 'data_xl_lessthans', and then rows will be reordered   

```r
data_xl <- data_xl %>%
  mutate(`D.d.i. code` = as.numeric(NA))

cat("\nNumber of columns:", ncol(data_xl), "\n") # 144
```

```
## 
## Number of columns: 145
```

```r
if (FALSE){
  head(data_xl$`D.d.i. code`)
  tail(data_xl$`D.d.i. code`)
  names(data_xl )
}
```


## 6. Add less-than columns   


### Check number of rows  

```r
# Using 'data_lessthans' created further up (1b)

# Check again
n1 <- nrow(data_lessthans)  # 29539
n2 <- nrow(data_xl)  # 29539

n_equal <- n1 == n2
if (!n_equal){
  cat("\n\n\n\n################################\n No. of rows differ! \n\n\n\n################################")
} else {
  cat("\nNumber of rows equal\n")
}
```

```
## 
## Number of rows equal
```

### Sort them equally

```r
# Sort them equally
data_xl <- data_xl %>%
  arrange(PARAM, STATION_CODE, LATIN_NAME, TISSUE_NAME, Basis)

data_lessthans <- data_lessthans %>% 
  arrange(PARAM, STATION_CODE, LATIN_NAME, TISSUE_NAME, Basis)

# Must be only zeros
ch1 <- sum(data_xl$PARAM != data_lessthans$PARAM)
ch2 <- sum(data_xl$STATION_CODE != data_lessthans$STATION_CODE)
ch3 <- sum(data_xl$LATIN_NAME != data_lessthans$LATIN_NAME)
ch4 <- sum(data_xl$TISSUE_NAME != data_lessthans$TISSUE_NAME)
ch5 <- sum(data_xl$Basis != data_lessthans$Basis)

all_match <- ch1 == 0 & ch2 == 0 & ch3 == 0 & ch4 == 0 & ch5 == 0
if (!all_match){
  cat("\n\n\n\n")
  message("################################")
  message("Lack of match in key variables! Check ch1-ch5")
  message("################################")
} else {
  cat("\nAll key variables matches\n")
}
```

```
## 
## All key variables matches
```

###  Intersperse empty columns  

```r
# Intersperse empty columns 
# colnames(data_lessthans)
extra_cols <- matrix(NA, nrow(data_lessthans), sum(isnum)) %>% as.data.frame()
colnames(extra_cols) <- paste0(colnames(data_lessthans)[isnum], "x")
# str(extra_cols)
data_lessthans2 <- cbind(data_lessthans, extra_cols)
# dput(sort(colnames(data_lessthans2)))
```

### Create data_xl_lessthans

```r
# Construct column names
# The ones with an "x" is just empty columns, they are tehre because there is an EQS column between each 
#   vakue column
txt1 <- rep(1981:last_year, each = 2)
txt2 <- rep(c("", "x"), length(txt1)/2)
cols <- paste0("Lt_", txt1, txt2)

# Select columns 
data_lessthans2 <- data_lessthans2[,cols]
# str(data_lessthans2)

if (n_equal & all_match){
  data_xl_lessthans <- cbind(data_xl, data_lessthans2)
  cat("\ndata_xl_lessthans created by adding less-than columns\n")
} else {
  cat("\nndata_xl_lessthans NOT created!\n")
}
```

```
## 
## data_xl_lessthans created by adding less-than columns
```

### Reorder rows in the data a bit   
We do this in order to get some much-used parameters in the top of the file,
 so Excel more easily put the right data type

```r
data_xl_lessthans$Substance.Group <- factor(data_xl_lessthans$Substance.Group) %>%
  forcats::fct_relevel("Support parameters",
                       "Metals and metalloids", "Chlorobiphenyls",
                       "Polycyclic aromatic hydrocarbons (PAHs)")

data_xl_lessthans <- data_xl_lessthans %>%
  arrange(Substance.Group, PARAM, STATION_CODE)
```

### Add values in D.d.i. code  
Still just an index number    
Making 'data_xl_lessthans' involves putting togethe
We have to do this before we have made 'data_xl_lessthans'  

```r
data_xl_lessthans <- data_xl_lessthans %>%
  mutate(`D.d.i. code` = 1:nrow(data_xl_lessthans))

if (FALSE){
  head(data_xl$`D.d.i. code`)
  tail(data_xl$`D.d.i. code`)
}
```

### Change tissue names to English  
To be on the safe side, added as TISSUE_NAME_new, which later is copied to TISSUE_NAME and then deleted

```r
xtabs(~addNA(TISSUE_NAME), data_xl_lessthans)
```

```
## addNA(TISSUE_NAME)
##            Blod             Egg           Galle           Lever          Muskel 
##             245             232              55            8068            1278 
## Whole soft body            <NA> 
##           14468               0
```

```r
data_xl_lessthans <- data_xl_lessthans %>%
  mutate(TISSUE_NAME_new = 
           case_when(TISSUE_NAME %in% "Blod" ~ "Blood",
                     grepl("Egg", TISSUE_NAME) ~ "Egg",
                     TISSUE_NAME %in% "Galle" ~ "Bile",
                     TISSUE_NAME %in% "Muskel" ~ "Muscle",
                     TISSUE_NAME %in% "Lever" ~ "Liver",
                     TISSUE_NAME %in% "WO" ~ "Whole organism",
                     TRUE ~ TISSUE_NAME)
  )

xtabs(~addNA(TISSUE_NAME_new), data_xl_lessthans)
```

```
## addNA(TISSUE_NAME_new)
##            Bile           Blood             Egg           Liver          Muscle 
##              55             245             232            8068            1278 
## Whole soft body            <NA> 
##           14468               0
```

```r
# Replace the original TISSUE_NAME
data_xl_lessthans$TISSUE_NAME <- data_xl_lessthans$TISSUE_NAME_new

# Delete TISSUE_NAME_new
data_xl_lessthans$TISSUE_NAME_new <- NULL


cat("\nNumber of columns:", ncol(data_xl_lessthans), "\n") # 222
```

```
## 
## Number of columns: 223
```

### Add Class for second last year (2017)  

```r
colnumber_value <- which(colnames(data_xl) == paste0("Yr_", last_year-1)); colnumber_value
```

```
## [1] 99
```

```r
value_secondlast <- data_xl[,colnumber_value]

colnumber_lessthan <- which(colnames(data_lessthans) == paste0("Lt_", last_year-1)); colnumber_lessthan
```

```
## [1] 44
```

```r
lessthan_lastyear <- data_xl[,colnumber_value]

# Less-thans are set a tad lower, to be put in the lower class
sel <- !is.na(lessthan_lastyear) & lessthan_lastyear
value_secondlast[sel] <- value_secondlast[sel] - 0.00001

# Just to check that we get the correct classes, i.e., if the conc. is on the limit, we get the upper class (using right = FALSE)
check_classes <- cut(value_secondlast/data_lessthans$Q95, breaks = c(-999999,1,2,5,10,20,999999), right = FALSE)
levels(check_classes)
```

```
## [1] "[-1e+06,1)" "[1,2)"      "[2,5)"      "[5,10)"     "[10,20)"   
## [6] "[20,1e+06)"
```

```r
class_secondlast <- cut(value_secondlast/data_xl$Q95, breaks = c(-999999,1,2,5,10,20,999999), right = FALSE, labels = FALSE)
# str(class_secondlast)
# summary(class_secondlast)
# table(addNA(class_secondlast))

# Make variable
data_xl_lessthans$Klasse.secondlast <- class_secondlast

# Tabulate
# table(addNA(data_xl$Klasse.lastyear))

# Set variable name
colnames(data_xl_lessthans)[ncol(data_xl_lessthans)] <- paste0("Klasse.", last_year-1, " NY")

nrow(data_xl_lessthans)  # 19304
```

```
## [1] 24346
```

```r
cat("\nNumber of columns:", ncol(data_xl_lessthans), "\n") # 219
```

```
## 
## Number of columns: 224
```

### Change a couple of sum PARAM names   

```r
sel <- data_xl_lessthans$PARAM %in% "KPAH"; sum(sel)
```

```
## [1] 81
```

```r
data_xl_lessthans$PARAM[sel] <- "PK_S"

sel <- data_xl_lessthans$PARAM %in% "PAH16"; sum(sel)
```

```
## [1] 81
```

```r
data_xl_lessthans$PARAM[sel] <- "PAHSS"
```

### Add coordinates  

```r
data_xl_lessthans <- data_xl_lessthans %>%
  safe_left_join(data_coordinates %>% select(STATION_CODE, Long, Lat),
                 by = "STATION_CODE",
                 check = "BCV",
                 na_matches = "never")
```




### Change column names  

```r
data_xl_lessthans_oldnames <- data_xl_lessthans
# data_xl_lessthans <- data_xl_lessthans_oldnames 

# Remove this column
data_xl_lessthans <- data_xl_lessthans %>%
  select(-Trends.2016.old)

# lookup file
df_colnames <- readxl::read_excel(
  "Input_data/Column headings v0409_2020_edit.xlsx") %>%
  mutate(
    New_name = case_when(
      is.na(Edited) ~ Norman,
      !is.na(Edited) ~ Edited),
    Current = ifelse(Current == "SD.2018", "SD_2018", Current)
  ) %>%
  arrange(Col_no)  # this defines the order of the columns

# Columns that will change names (and order) are all in the left part 

# Left part of file: all columns until column before Lt_1981
n1 <- 1
n2 <- which(names(data_xl_lessthans) %in% "Lt_1981") - 1   # all columns until column before Lt_1981
# Right part of file
n3 <- which(names(data_xl_lessthans) %in% "Lt_1981")       # Lt_1981
n4 <- ncol(data_xl_lessthans)

# Original names
orig_names <- names(data_xl_lessthans)[n1:n2]   # Column names in left part of the file   

# Check that all original names are found in lookup file
check <- orig_names %in% as.data.frame(df_colnames)$Current
if (sum(!check) > 0){
  stop("Some column names are not found in the lookup file! ")
  orig_names[!check]
}

# Pick and sort 
old_names_ordered <- df_colnames %>% 
  filter(!is.na(Col_no)) %>% 
  pull(Current)
new_names <- df_colnames %>% 
  filter(!is.na(Col_no)) %>% 
  pull(New_name)

# Reorder and rename left part of table
df_left_part <- data_xl_lessthans[old_names_ordered]
names(df_left_part) <- new_names

# Get right part of table
df_right_part <- data_xl_lessthans[n3:n4]

# Put it all together
data_xl_lessthans <- bind_cols(df_left_part, df_right_part)

if (FALSE){
  names(data_xl_lessthans)
}

# Write out
x <- names(data_xl_lessthans)
cat("Names of first columns: \n")
```

```
## Names of first columns:
```

```r
new_names[1:22]
```

```
##  [1] "Parameter Code"           "Parameter Name (short)"  
##  [3] "IUPAC"                    "CAS"                     
##  [5] "Parameter Name (long)"    "Parameter Group"         
##  [7] "Unit"                     "Station Code"            
##  [9] "Station Name"             "Area"                    
## [11] "County"                   "Water Region"            
## [13] "Water Body ID"            "Water Body name"         
## [15] "Species"                  "Tissue"                  
## [17] "Basis"                    "Reference stations"      
## [19] "Reference station count"  "Reference value count"   
## [21] "Reference station median" "PROREF"
```

```r
cat("\n")
```

```r
cat("Names of columns of median values: \n")
```

```
## Names of columns of median values:
```

```r
cat("  ", x[23], "etc. +", x[24], "etc. \n")
```

```
##    V80 etc. + Q80 etc.
```

```r
cat("\n")
```

```r
cat("Names of last columns: \n")
```

```
## Names of last columns:
```

```r
#  names(data_xl_lessthans)
i1 <- max(which(grepl("^Q[0-9][0-9]$", x))) + 1
i2 <- min(which(grepl("Lt_", x))) - 1
x[i1:i2]
```

```
##  [1] "N_string_last year"                  
##  [2] "SD last year"                        
##  [3] "PROREF-class last year"              
##  [4] "EQS-class last year"                 
##  [5] "EQS-threshold last year"             
##  [6] "N_string this year"                  
##  [7] "SD this year"                        
##  [8] "PROREF-class this year"              
##  [9] "EQS-class this year"                 
## [10] "EQS-threshold this year"             
## [11] "Dummy-1"                             
## [12] "Trend p(long) this year"             
## [13] "Detectable % change(long) this year" 
## [14] "First Year(long) this year"          
## [15] "Last Year(long) this year"           
## [16] "No. of Years(long) this year"        
## [17] "Trend p(short) this year"            
## [18] "Detectable % change(short) this year"
## [19] "First Year(short) this year"         
## [20] "Last Year(short) this year"          
## [21] "No. of Years(short) this year"       
## [22] "Trends this year"                    
## [23] "Trend. p(long) last year"            
## [24] "Detectable % change(long) last year" 
## [25] "First. Year(long) last year"         
## [26] "Last. Year(long) last year"          
## [27] "No. of Years(long) last year"        
## [28] "Trend. p(short) last year"           
## [29] "Detectable % change(short) last year"
## [30] "First. Year(short) last year"        
## [31] "Last. Year(short) last year"         
## [32] "No. of Years(short) last year"       
## [33] "Trends last year"                    
## [34] "Last two years"                      
## [35] "LOQ last year"                       
## [36] "LOQ this year"                       
## [37] "TREND_CHANGE last year-this year"    
## [38] "PROREF_CHANGE last year-this year"   
## [39] "EQS_CHANGE last year-this year"      
## [40] "EAC_CHANGE last year-this year"      
## [41] "D.d.i. code"                         
## [42] "N>LOQ[min-maks]"
```

### For checking

```r
if (FALSE){
  
  data_xl_lessthans[c("Parameter Code", "Parameter Group", "V81", "V19")] %>% View()
  
  data_xl_lessthans[c("Parameter Code", "V81", "V19")] %>%
    group_by(`Parameter Code`) %>%
    summarise_all(.funs = function(x) sum(is.na(x))) %>%
    mutate(Sum = V81 + V19) %>%
    arrange(desc(Sum))
  
  data_xl_lessthans[c("Parameter Code", "V81", "V19")] %>%
    group_by(`Parameter Code`) %>%
    summarise_all(.funs = function(x) sum(is.na(x))) %>%
    mutate(Sum = V81 + V19) %>%
    arrange(desc(Sum))
  
  names(data_xl_lessthans_oldnames)
  data_xl_lessthans_oldnames %>% 
    select(PARAM, LATIN_NAME, TISSUE_NAME, Basis, Yr_2019, N_string, SD_last) %>%
    View()

}
```

## 7. Save 'data_xl_lessthans' as text and RDS file  

```r
#
# Existing file names (dates and versions)
#
fns <- dir("Big_excel_table", pattern = "Data_xl_.+.csv") %>% rev()
fns_date <- substr(fns, 9, 18)
fns_ver <- stringr::str_extract(fns, "ver[0-9]+") %>% stringr::str_extract("[0-9]+") %>% as.numeric()
file_versions <- tibble(Filename = fns, Date = fns_date, Version = fns_ver) %>%
  arrange(desc(Date), desc(Version))
file_versions

#
# csv
#

# Version number to use
# - if file with same date exists in the folder, we increase the version by 1
# - if file with same date does not exist, we set version to 1
fn_version <- case_when(
  file_date == file_versions$Date[1] ~ file_versions$Version[1] + 1,
  file_date == file_versions$Date[1] ~ 1
)
fn_version_txt <- sprintf("%02i", fn_version)

# File name to use
fn <- paste0("Data_xl_", file_date, "_ver", fn_version_txt, ".csv")
fn_full <- paste0("Big_excel_table/", fn)


if (fn %in% dir("Big_excel_table")){
  cat("File", fn, "already exists! File will not be overwritten.")
} else {
  # Save CSV
  if (decimal_comma){
    write.csv2(data_xl_lessthans, file = fn_full, quote = FALSE, na = "", row.names = FALSE)
  } else {
    write.csv(data_xl_lessthans, file = fn_full, quote = FALSE, na = "", row.names = FALSE)
  }
  cat("Text file", sQuote(fn), "written to folder 'Big_Excel_table'\n")
  # Save RDS
  saveRDS(data_xl_lessthans_oldnames, sub("csv", "rds", fn_full))
  cat("R data file", sQuote(sub("csv", "rds", fn)), "written to folder 'Big_Excel_table'\n")
}

#
# For map-making
#
fn_stations <- paste0("201_Stations_", last_year, ".xlsx")
data_xlvalues1 %>%
  filter(MYEAR %in% last_year) %>%
  count(STATION_CODE, LATIN_NAME) %>%
  arrange(STATION_CODE) %>% 
  writexl::write_xlsx(paste0("Data/", fn_stations))
cat("\n")
cat("Excel file", sQuote(fn_stations), "(one line per station) written to folder 'Data'\n")
```

```
## # A tibble: 15 x 3
##    Filename                     Date       Version
##    <chr>                        <chr>        <dbl>
##  1 Data_xl_2020-08-05_ver11.csv 2020-08-05      11
##  2 Data_xl_2020-08-05_ver10.csv 2020-08-05      10
##  3 Data_xl_2020-08-05_ver09.csv 2020-08-05       9
##  4 Data_xl_2020-08-05_ver08.csv 2020-08-05       8
##  5 Data_xl_2020-08-05_ver07.csv 2020-08-05       7
##  6 Data_xl_2020-08-05_ver06.csv 2020-08-05       6
##  7 Data_xl_2020-08-05_ver05.csv 2020-08-05       5
##  8 Data_xl_2020-08-05_ver04.csv 2020-08-05       4
##  9 Data_xl_2020-08-05_ver03.csv 2020-08-05       3
## 10 Data_xl_2020-08-05_ver02.csv 2020-08-05       2
## 11 Data_xl_2020-08-05_ver01.csv 2020-08-05       1
## 12 Data_xl_2020-07-03.csv       2020-07-03      NA
## 13 Data_xl_2020-06-12.csv       2020-06-12      NA
## 14 Data_xl_2020-05-29.csv       2020-05-29      NA
## 15 Data_xl_2020-04-30.csv       2020-04-30      NA
## Text file 'Data_xl_2020-08-05_ver12.csv' written to folder 'Big_Excel_table'
## R data file 'Data_xl_2020-08-05_ver12.rds' written to folder 'Big_Excel_table'
## 
## Excel file '201_Stations_2019.xlsx' (one line per station) written to folder 'Data'
```
### For checking (old) files  

```r
#
# NOTE: don't run all of the stuff inside the brackets
# These are code snippets, so you are meant to pick and choose
# (for instance, it doesn't make sense to make 'check' first by reading the text file,
#   and then by reading the rds file. You only need to do one of them.)
#
if (FALSE){
  #
  # Existing file names (dates and versions)
  #
  fns <- dir("Big_excel_table", pattern = "Data_xl_.+.csv") %>% rev()
  # OR:
  # fns <- dir("Big_excel_table", pattern = "Data_xl_.+.rds") %>% rev()
  fns_date <- substr(fns, 9, 18)
  fns_ver <- stringr::str_extract(fns, "ver[0-9]+") %>% stringr::str_extract("[0-9]+") %>% as.numeric()
  file_versions <- tibble(Filename = fns, Date = fns_date, Version = fns_ver) %>%
    arrange(desc(Date), desc(Version))
  file_versions
  
  # Get most recet file (based on date and version in file name)
  fn <- paste0("Big_excel_table/", file_versions$Filename[1])
  fn

  
  # read R data (fast)
  check <- readRDS(file = sub(".csv", ".rds", fn, fixed = TRUE))

  # OR read csv (slow)
  check <- read.csv2(file = fn)

  # Check header of csv:
  # readLines(fn, n = 1)

  # All column names
  names(check)
  
  # Check data for all TBT
  check %>%
    filter(PARAM == "TBT" & Basis == "WW") %>%
    select(PARAM, Substance.Group, LATIN_NAME, TISSUE_NAME, STATION_CODE, Basis, 
         Yr_2018, Yr_2019, N_string, SD_last, EQS) %>%
    View()

  # Check data for all TBT
  check %>%
    filter(PARAM == "HG" & Basis == "WW" & STATION_CODE == "30B") %>%
    select(PARAM, Substance.Group, LATIN_NAME, TISSUE_NAME, STATION_CODE, Basis, 
         Yr_2018, Yr_2019, N_string, SD_last, EQS, Trends.2019) %>%
    View()

  # Check all data for one snail station
  check %>%
    filter(Basis == "WW" & STATION_CODE == "71G") %>%
    select(PARAM, Substance.Group, LATIN_NAME, TISSUE_NAME, STATION_CODE, Basis, 
         Yr_2003, Yr_2010, Yr_2014, Yr_2018, Yr_2019, N_string, SD_last, EQS) %>%
    View("11G")

  check %>%
    filter(Basis == "WW" & STATION_CODE == "30B" & PARAM == "HG") %>%
    select(PARAM, Substance.Group, LATIN_NAME, TISSUE_NAME, STATION_CODE, Basis, 
           Yr_2003, Yr_2010, Yr_2014, Yr_2018, Yr_2019, N_string, SD_last, EQS, Trends.2019) %>%
    View("check")
  
  # Read older version (check2) and compare number of rows and column 1:22 between check and check2: 
  fn2 <- paste0("Big_excel_table/", file_versions$Filename[2])
  check2 <- read.csv2(file = fn2)
  dim(check)
  dim(check2)
  i <- 1
  for (i in 1:22){
    cat(i, ":", sum(!check[[i]] == check2[[i]], na.rm = TRUE), "\n")
  }

    # colnames(check)
  colnames(check)[1:20]
  check <- check %>%
    rename(PARAM = Parameter.Code,
           STATION_CODE = Station.Code,
           TISSUE_NAME = Tissue)
  
  xtabs(~addNA(CLASS_CHANGE), check)
  
  check %>%
    xtabs(~addNA(CLASS_CHANGE), check)

  
  
}
```



## 8. How to change the .csv file from this script into the finished Excel file     
This needs to be done in Excel, using some macros (stored in 'Data_xl_TEMPLATE.xlsm'). Also, opening the data in MS Excel is not straightforward, due to Linux/Windows differences in encodings.      

NOTE 1 (IMPORTANT!): Some variables in the macros must be adjusted EVERY YEAR, in order to accomodate for an increasing number of columns every year. Otherwise the macros in (4) will not work at all. The instructions for this is given in the macro script file in Excel (View : Macros : View Macros : [choose a random macro] : Edit)
  
NOTE 2: Step 1 requires Notepad++, which can be downloaded for free from https://notepad-plus-plus.org/    
  
1. Download the produced file, named "Data_xl_[some date].csv", from Juputerhub to your own computer (in Jupyterlab, right click the file and "download"). For instance: Data_xl_2020-07-03.csv  
    * ...if you wondered, [some date] means the date of the latest file, e.g. '2020-07-03' 
    * Also download the file , if you haven't done so before  
    * Both should be put in the same directory. For DHJ: 'Milkys_pc/Files_from_Jupyterhub_2019/Big_excel_table'
2. Open the file in Excel in the following way:   
    * First, make a copy of 'Data_xl_TEMPLATE.xlsm' (Ctrl-C, Ctrl-V) and rename it with the same name as your csv file, except that you replace .csv by .xlsm (mark the csv file and press F2, Ctrl-C; mark the xlsm file and press F2, Ctrl-V)    
    * Open this file (in Excel)    
    * Open the csv file (the one from step 1) in Notepad++ (right-click and select Notepad++)
    * Mark all (Ctrl-A) and copy it (Ctrl-C)  
    * Paste (Ctrl-V) it into the top left cell in an empty sheet (e.g. sheet "X") in the Excel file  
    * (The reason for this seemingly pointless detour is to handle encodings for Norwegian characters)
3. Now your data need to be split into columns. 
    * Mark the first column (A). 
    * Choose Data : Text to columns
    * Choose "Delimited" for file type, then click Next
    * Choose "Semicolon", then click Next
    * Click "Advanced" and make sure that decimal separetor is ",".  Click OK and hit "Finish".   
4. Go to View:Macros (may also be in Developer:Macros, depending on Excel setup) and run the macros given below (mark the macro you want and click "Run").
NOTE: if you have filtered the data, turn OFF all filtering of data before running any of the macros, or Excel may crash...
    * __Format_Cells_Lessthan_Shade_EQS__. This takes ca. 5 minutes
      (this formats cells with less-than values, shades cells according to Proref,
      and formats/and sets colors for the EQS dots)   
    * Mark the first cell in the Trends.2019 column (col. DT) and
      run __'Set_character_1_and_3_to_Wingdings'__  
    * Do the same for Trends.2018 (col. EE). Shortcut:  Ctrl+Y
    * Mark the first cell in the TREND_CHANGE column (col. EI) and
      run __'Set_character_1_3_8_and_10_to_Wingdings'__  
5. Save excel file  

  
EXPECTED COLUMN POSITIONS FOR EXCEL MACROS TO WORK   
(these will change every year, so the macros must also be updated - see comments in macros)
2020 VERSION (last year = 2019)   
- Column Y = Yr1981  
- Column CW = Yr2019  
- Column EO = Lt_1981  
- Column HM = Lt_2019  
- Distance from Y to EO (Yr1981 to Lt_1981) = 120 columns  
