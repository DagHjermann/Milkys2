---
title: "110_Medians_and PROREF"
output: 
  html_document:
    keep_md: true
    toc: true
---

Condenses data to annual medians (per parameter/station/tissue)  
* I.e., this is the main source of time series data  
  
Some data series are also 'homogenized' here  
* E.g., we make a single time series for some data with different STATION_CODE  

**NOTE: 'Fixing data for time series' must be identical in script 110 and 111**

## 1. Packages

```r
library(readxl)
library(tidyr)

#
# Dropping dtplyr (it only ses 20 seconds or so in this script)
#
# install.packages("dtplyr")
# library(data.table)
# library(dtplyr)

library(dplyr, warn.conflicts = FALSE)
library(purrr)

source("002_Utility_functions.R")
source("110_Medians_and_PROREF_functions.R")
```



## 2. Data

### Main data  
Read and reformat the most recent data (by default)  

```r

# If we have NOT length-adjusted the last year's data:
# filepattern <- "101_data_updated_"         # entire file name except date and extension

# Normally, if we have length-adjusted the last year's data:
filepattern <- "109_adjusted_data_"       # entire file name except date and extension

filenumber <- 1                           # filenumber = 1 means "read the newest file"

files <- dir("Data", pattern = filepattern) %>% rev()

data_list <- read_rds_file("Data",
                     files, 
                     filenumber = filenumber,   # "1" gets the newest file   
                     get_date = TRUE, time_since_modified = TRUE)
## File '109_adjusted_data_2022-09-23.rds' (file number 1) has been read 
##   This is the newest file. If you want to read an older file, put a different 'filenumber' 
## 
## Time since this file was modified: 
## Time difference of 13.8049 days

data_all <- data_list$data

# The file_date text will be used in part 10, when we save the resulting file
cat("File date text:", data_list$file_date, "\n")
## File date text: 2022-09-23
```


### Homogenize time series  
Change STATION_CODE, in order to make a single time series for data with different STATION_CODE that in practice should count as the same station   
* Fixing station 227G1 and 227G2 (shall count as 227G)  
* Fixing station 36A and 36A1 (36A1 shall count as 36A)  
  
Also combines PARAM = VDSI and PARAM = Intersex to PARAM = "VDSI/Intersex" for station 71G  


```r
data_all <- homogenize_series(data_all)
```

```
## Fixing station 227G1 and 227G2 (shall count as 227G) 
## - changed STATION_CODE for 1397 rows of data 
## 
## Fixing station 36A and 36A1 (36A1 shall count as 36A) 
## - changed STATION_CODE for 2069 rows of data 
## 
## Fix VDSI and Intersex at station 71G (both shall count as PARAM = 'VDSI/Intersex') 
## - changed PARAM for 14 rows of data
```

### Check: Is C/N lacking any years?    

```r
if (FALSE) { 
  
  # Check %C by station - good coverage 2015-2019, bad coverage after that
  data_all %>%
    filter(PARAM %in% "% C" & MYEAR >= 2000) %>%
    xtabs(~STATION_CODE + MYEAR, .)
}

# 
data_all %>%
  filter(PARAM %in% "% C" & MYEAR >= 2000) %>%
  xtabs(~is.na(VALUE_WW) + MYEAR, .)
```

```
##                MYEAR
## is.na(VALUE_WW) 2015 2016 2017 2018 2019 2020 2021
##           FALSE  309  298  359  341  305  340  328
```

```r
data_all %>%
  filter(PARAM %in% "C/N" & MYEAR >= 2000) %>%
  xtabs(~is.na(VALUE_WW) + MYEAR, .)
```

```
##                MYEAR
## is.na(VALUE_WW) 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021
##           FALSE  238  249  272  309  298  359  341  305  340  328
```

```r
if (FALSE){
  # Dry-weight data: no different
  data_all %>%
    filter(PARAM %in% "C/N" & MYEAR >= 2000) %>%
    xtabs(~is.na(VALUE_DW) + MYEAR, .)
}
```


### Proref values  
We do not update Proref values, as we don't want to have a "moving target". However, a selection of wet-weight Proref values were re-estimated for the Proref paper, so we need to use these. Thus we use a combination of   
- proref_old, which are the Proref values used in the report for the 2017 data - is used for all parameter/basis combinattions that are not in proref_paper  
- proref_paper, which are the Proref values used in the Proref paper (a selection of parameters, and wet weigth only)  
These are combined to proref_updated02, which added to the data in part 4    

```r
# Old ones 
proref_old <- read_excel("Input_data/Proref_report_2017.xlsx")
proref_paper <- read_excel("Input_data/Proref_paper.xlsx")

# proref_updated01 = old data set, with selected columns,
#   and removing the rows that were produced for the paper (proref_ww)
proref_updated01 <- proref_old %>%
  # Select columns to keep
  select(PARAM, LATIN_NAME, TISSUE_NAME, Basis, Stations, N_stations, N, Median, Q95) %>%
  # Pick rows that are *not* in proref_paper
  anti_join(proref_paper %>% select(PARAM, LATIN_NAME, TISSUE_NAME, Basis),
            by = c("PARAM", "LATIN_NAME", "TISSUE_NAME", "Basis")
            )

# proref_updated02 - adding the rows from proref_paper
proref_updated02 <- proref_updated01 %>%
  bind_rows(
    proref_paper %>%
      select(PARAM, LATIN_NAME, TISSUE_NAME, Basis, Stations, N_stations, N, Median, Q95)
  )
```
  
For Norman:  
1.	Hvilke stasjoner inngikk A (trengs ikke for B): 
    - 'Stations'   
2.	Antall stasjoner for A og B  
    - A: 'N_stations', B: mangler     
3.	Antall verdier for A og B   
    - A: 'N', B: Mangler   
4.	Antall verdier over LOQ for A og B    
    - Mangler  
5.	PROREF verdien (gjelder bare A selvfølgelig)    
    - 

```r
proref_old %>% names()
```

```
##  [1] "PARAM"       "LATIN_NAME"  "TISSUE_NAME" "Basis"       "Variable"   
##  [6] "UNIT"        "Years_start" "Years_end"   "Stations"    "N_stations" 
## [11] "N"           "Median"      "Q90"         "Q95"         "Max"        
## [16] "CI90"        "CI95"
```



### Fix (remove) some values with one station and PROREF = 0  
CB81, CB126 and CB169 in blue mussel   

```r
sel <- with(proref_updated02, Q95 %in% 0 & N_stations %in% 1)
# proref_updated02[sel,]
proref_updated02$Q95[sel] <- NA
proref_updated02$Stations[sel] <- NA
proref_updated02$N_stations[sel] <- NA
proref_updated02$N[sel] <- NA

cat(sum(sel), "values set to NA \n")
```

```
## 3 values set to NA
```


### Check Proref  

```r
check <- proref_updated02 %>%
  count(PARAM, LATIN_NAME, TISSUE_NAME, Basis)

if (sum(check$n > 1) == 0){
  cat("All OK - only one Proref value per PARAM, LATIN_NAME, TISSUE_NAME, Basis combination  \n")
} else {
  cat("Warning: more than one Proref value per PARAM, LATIN_NAME, TISSUE_NAME, Basis combination. Must be fixed!  \n")
}
```

```
## All OK - only one Proref value per PARAM, LATIN_NAME, TISSUE_NAME, Basis combination
```


## 3. Summarise data for each year/station/parameter  

### Calculate medians using dtplyr  

```r
# data_all_dt <- lazy_dt(data_all)

t0 <- Sys.time()

data_med_1a <- data_all %>% 
  group_by(MYEAR, STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM, UNIT) %>% 
  summarise_at(vars(DRYWT, FAT_PERC, VALUE_WW, VALUE_DW, VALUE_FB, VALUE_WWa, VALUE_DWa, VALUE_FBa), 
               median, 
               na.rm = TRUE) %>%
  as_tibble()

Sys.time() - t0  # 19 secs (took 0.19 sec with data.table)
```

```
## Time difference of 21.38031 secs
```


### Calculate N, Det_limit, Over_LOQ, min/max and quantiles  

```r
t0 <- Sys.time()

data_med_1b <- data_all %>%
  group_by(MYEAR, STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM, UNIT) %>%
  summarise(
    N = n(), 
    Det_limit = median(VALUE_WW[!is.na(FLAG1)]), 
    Over_LOQ = sum(is.na(FLAG1)),
    Value_min = min(VALUE_WW, na.rm = TRUE),
    Value_p25 = quantile(VALUE_WW, probs = 0.25, na.rm = TRUE),
    Value_p75 = quantile(VALUE_WW, probs = 0.75, na.rm = TRUE),
    Value_max = max(VALUE_WW, na.rm = TRUE),
    .groups = "drop") %>%
  as_tibble()

Sys.time() - t0  # 36 secs
```

```
## Time difference of 34.51405 secs
```

### Combine data_med_1 and data_med_2    
Add N, Det_limit and Over_LOQ to the data set with medians  

```r
# Make sure the data sets are sorted the same way
data_med_1a <- data_med_1a %>% arrange(MYEAR, STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM, UNIT)
data_med_1b <- data_med_1b %>% arrange(MYEAR, STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM, UNIT)

# Make sure the data sets have the same number of rows
if (nrow(data_med_1a) == nrow(data_med_1b)){
  # Join data by adding the N, Det_limit, Over_LOQ columns 
  data_med_1 <- bind_cols(
    data_med_1a,
    data_med_1b %>% select(N, Det_limit, Over_LOQ, Value_min, Value_p25, Value_p75, Value_max)
  )
} else {
  cat("data_med_1a and data_med_1b have different number of rows! Must be fixed, or use left_join(). \n")
}
```


## 3B. Add extra PAH metabolite data downloaded from ICES  

* Parameters: PYR1OH, BAP3OH, PA1OH, i.e. the unadjusted/unnormalised values 
    - the 'O' versions, i.e. the adjusted ones, are already there 
* Existing in the data, but some years (e.g. 2001-2004) lacking  
* Downloading these data from ICES    
* NOTE: These data should be added to the Nivabasen! But that is for later  

### Downloading data  

```r
# The ICES database sometimes hangs, therefore we can also get data from the previous dataset  
# - see end of next chunk  
get_data_from_ICES <- FALSE 

if (get_data_from_ICES){
  
  # PYR1OH data for all years
  df1 <- get_ices_biotadata(param = "PYR1OH", country = 58)
  df2 <- get_ices_biotadata(param = "BAP3OH", country = 58)
  df3 <- get_ices_biotadata(param = "PA1OH", country = 58)
  
  nrow(df1); nrow(df2); nrow(df3);
  
  cat("Check PYR1OH data: \n\n")
  xtabs(~STATN + MYEAR, df1)
  
  if (FALSE){
    df1 %>% 
      count(STATN, MYEAR) %>%
      arrange(STATN, MYEAR)
  }
  
}
```

### Calculate median data set      


```r
if (get_data_from_ICES){
  
  extract_first_word <- function(string, pattern_word_break = "[[[:blank:]]"){
    m <- regexpr(pattern_word_break, string)
    substr(string, 1, m-1)
  }
  # Test (vectorised)
  # extract_first_word(c("36B Færder area - torsk lever 7 og 11", "36B2 Færder area - torsk muskel 5")) 
  
  data_medians_ices_1 <- bind_rows(df1, df2, df3) %>%
    mutate(STATION_CODE = extract_first_word(STATN),
           TISSUE_NAME = "Galle",
           UNIT = case_when(
             MUNIT %in% 'ug/kg' ~ "UG_P_KG",
             MUNIT %in% 'ng/g' ~ "UG_P_KG",
             TRUE ~ as.character(NA))
    ) %>%
    rename(LATIN_NAME = Species,
           FLAG1 = QFLAG) %>%
    # Calculate median perstation/year/parameter
    group_by(MYEAR, STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM, UNIT) %>%
    summarise(N = n(),
              VALUE_WW = median(Value),
              Over_LOQ = sum(is.na(FLAG1)),
              .groups = "drop"
    )
  
  if (sum(is.na(data_medians_ices_1$UNIT))>0){
    stop("There are unidentified units!")
    table(bind_rows(df1, df2, df3)$MUNIT)
  }
  
} else {
  
  # Just use the previous data  
  
  data_med_prev <- readRDS("Data/110_mediandata_updated_2022-09-01.rds") %>%
    filter(PARAM %in% c("PYR1OH", "BAP3OH", "PA1OH") & Basis %in% "WW")
  
  data_medians_ices_1 <- data_med_prev %>%
    select(MYEAR, STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM, UNIT,
           DRYWT, FAT_PERC, 
           VALUE_WW = Value, N = N_median, Over_LOQ)
  
}
```

### Combine with the rest   

```r
if (FALSE){
  
  # check variables
  n1 <- data_med_1 %>% names()
  n2 <- data_medians_ices_1 %>% names()
  setdiff(n1, n2)   # names in 1 that are not in 2
  setdiff(n2, n1)   # names in 2 that are not in 1
  
  # check years  
  data_med_1 %>% 
    filter(PARAM == "PYR1OH") %>%
    xtabs(~STATION_CODE + MYEAR, .)
  data_medians_ices_1 %>% 
    filter(PARAM == "PYR1OH") %>%
    xtabs(~STATION_CODE + MYEAR, .)

}


# Add 'data_already_there' variable  
# If data_already_there = 1, the data have already been added 
data_medians_ices_2 <- data_medians_ices_1 %>% 
    left_join(
       data_med_1 %>% 
        filter(PARAM %in% c("PYR1OH", "BAP3OH", "PA1OH")) %>%
        select(PARAM, MYEAR, STATION_CODE) %>%
        mutate(data_already_there = 1)
    )
```

```
## Joining, by = c("MYEAR", "STATION_CODE", "PARAM")
```

```r
sum(!is.na(data_medians_ices_2$data_already_there))
```

```
## [1] 115
```

```r
mean(!is.na(data_medians_ices_2$data_already_there))
```

```
## [1] 0.4509804
```

```r
# Data to add
data_medians_ices_3 <- data_medians_ices_2 %>%
  filter(is.na(data_already_there))

data_medians_ices_3 %>% 
  filter(PARAM == "PYR1OH") %>% # View()
  xtabs(~STATION_CODE + MYEAR, .)
```

```
##             MYEAR
## STATION_CODE 1998 1999 2000 2001 2002 2003 2004 2008 2009 2014
##         10B     0    0    1    1    0    0    0    0    0    0
##         10F     0    0    1    1    0    0    0    0    0    0
##         15B     1    1    0    1    1    1    1    1    0    1
##         15F     0    1    0    1    0    0    0    0    0    0
##         21F     0    1    0    0    0    0    0    0    0    0
##         23B     1    1    1    1    1    1    1    1    1    1
##         30B     1    1    1    1    1    1    1    1    1    1
##         36B     1    1    1    1    0    0    0    0    0    0
##         36F     0    1    1    1    0    0    0    0    0    0
##         53B     1    1    1    1    1    1    1    1    1    1
##         67B     1    1    1    1    0    0    0    0    0    0
##         98B2    0    0    1    1    0    0    0    0    0    0
##         98F2    0    0    1    1    0    0    0    0    0    0
```

```r
if (FALSE){
  # for comparison
  data_medians_ices_2 %>% 
    filter(PARAM == "PYR1OH") %>%
    xtabs(~STATION_CODE + MYEAR, .)
}
```

### Add PAH met.data  

```r
data_med_1_added_pahmetab <- bind_rows(
  data_med_1,
  data_medians_ices_3
)
```


## 3C. Fix "SCCP eksl. LOQ" and "MCCP eksl. LOQ"  

* For these, FLAG1 is not used in the raw data, and VALUE_WW can be 0   
* Example: 30B in 2021 has values 7.0, 8.1, 8.5, + 12 cases where value = 0   
* For cases where >= 50% of the data are 0, we decide to use lowest data >0 and set Over_LOQ based on number of values >0     
    - if no data is over zero, we use value = 0 


```r
params <- c("SCCP eksl. LOQ", "MCCP eksl. LOQ")

data_med_ccp <- list()

for (param in params){

  # param <- "SCCP eksl. LOQ"
  
  df1 <- data_all %>%
    filter(PARAM %in% param) 
  xtabs(~STATION_CODE + MYEAR, df1)
  df2 <- df1 %>%
    group_by(MYEAR, STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM, UNIT) %>% 
    summarise(
      N = n(),
      Over_LOQ = sum(VALUE_WW > 0),
      Median = median(VALUE_WW),
      Value_min = min(VALUE_WW, na.rm = TRUE),
      Value_p25 = quantile(VALUE_WW, probs = 0.25, na.rm = TRUE),
      Value_p75 = quantile(VALUE_WW, probs = 0.75, na.rm = TRUE),
      Value_max = max(VALUE_WW, na.rm = TRUE),
      Min_over_zero = min(VALUE_WW[VALUE_WW > 0]),
      DRYWT = median(DRYWT),
      FAT_PERC = median(FAT_PERC),
      .groups = "drop"
    ) %>%
    mutate(
      VALUE_WW = case_when(
        Over_LOQ >= N/2 ~ Median,
        Over_LOQ < N/2 & Over_LOQ > 0 ~ Min_over_zero,
        Over_LOQ == 0 ~ 0),
      Det_limit = Min_over_zero
    )
  
  data_med_ccp[[param]] <- df2 %>%
    select(-Median, -Min_over_zero)
  
}

data_med_1_fixed_ccp <- data_med_1_added_pahmetab %>%
  filter(!PARAM %in% params) %>%
  bind_rows(data_med_ccp[["SCCP eksl. LOQ"]]) %>%
  bind_rows(data_med_ccp[["MCCP eksl. LOQ"]])
```


## 4. Add Proref  

### Add "VDSI/Intersex" to PROREF data 

```r
# Pick one VDSI line and change it
sel <- proref_updated02$PARAM == "VDSI" & proref_updated02$Basis == "WW"; sum(sel)
```

```
## [1] 1
```

```r
proref_to_add <- proref_updated02[sel,]
proref_to_add$PARAM <- "VDSI/Intersex"

# Add 
proref_updated03 <- bind_rows(
  proref_updated02,
  proref_to_add
)
```

### Save    

```r
write.csv(proref_updated03 %>% rename(Proref = Q95), "Input_data/Lookup_tables/Lookup_proref.csv")
```


### Add PROREF data to medians   

```r
data_med_2 <- data_med_1_fixed_ccp %>%
  pivot_longer(
    cols = c(VALUE_WW, VALUE_DW, VALUE_FB, VALUE_WWa, VALUE_DWa, VALUE_FBa), 
    names_to = "Basis", 
    values_to = "Value") %>%
  mutate(Basis = substr(Basis, start = 7, stop = 100))

data_med_3 <- data_med_2 %>%
  rename(N_median = N) %>%
  left_join(proref_updated03 %>% rename(Proref_N = N), 
            by = c("LATIN_NAME", "TISSUE_NAME", "PARAM", "Basis"))

nrow(data_med_2)  # 525960
```

```
## [1] 613884
```

```r
nrow(data_med_3)  # 525960
```

```
## [1] 613884
```

### Remove SCCP/MCCP prorefs (for now)   

But there are none  


```r
sel <- with(data_med_3, grepl("CCP eksl. LOQ", PARAM))

# View(data_med_3[sel,])
# table(data_med_3[sel,]$Q95)
```


### Checks 

#### Check 19N   
Should be NA for all Q95 values     

```r
data_med_3 %>%
  filter(STATION_CODE == "19N") %>%
  xtabs(~is.na(Q95), .)
```

```
## is.na(Q95)
## TRUE 
## 6612
```

#### Check C/N   
Should be NA for all Q95 values     

```r
data_med_3 %>%
  filter(STATION_CODE == "19N") %>%
  xtabs(~is.na(Q95), .)
```

```
## is.na(Q95)
## TRUE 
## 6612
```
### Remove names  

```r
names(data_med_3$Value_p25) <- NULL
names(data_med_3$Value_p75) <- NULL
```


## 5. Save  

```r
filename <- paste0("Data/110_mediandata_updated_", data_list$file_date, ".rds")

saveRDS(data_med_3, filename)

cat("Data of medians saved as: \n")
```

```
## Data of medians saved as:
```

```r
cat(" ", filename, "\n")
```

```
##   Data/110_mediandata_updated_2022-09-23.rds
```

### Read back  

```r
if (FALSE){
  data_med_3 <- readRDS("Data/110_mediandata_updated_2022-09-23.rds")
}
```




