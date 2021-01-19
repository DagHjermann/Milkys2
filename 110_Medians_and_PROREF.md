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
# Dropping dtplyr (it only saves 20 seconds or so in this script)
#
# install.packages("dtplyr")
# library(data.table)
# library(dtplyr)

library(dplyr, warn.conflicts = FALSE)
library(purrr)

source("110_Medians_and_PROREF_functions.R")
```



## 2. Data

### Main data  
Read and reformat the most recent data (by default)  

```r

files <- dir("Data", pattern = "109_adjusted_data") %>% rev()

cat("Reading the last file downloaded:")
## Reading the last file downloaded:
cat("\n", files[1])
## 
##  109_adjusted_data_2020-08-05.rds
cat("\n")
cat("If you want to read a different file, replace 'files[1]' with the file you want")
## If you want to read a different file, replace 'files[1]' with the file you want
cat("\n\n")

filename <- files[1]
cat("Time since this file was modified: \n")
## Time since this file was modified:
Sys.time() - file.info(paste0("Data/", filename))$mtime
## Time difference of 25.02326 days

data_all <- readRDS(paste0("Data/", filename))

# We save the date part of the text (e.g., '2020-04-23')
# This will be used in part 10, when we save the resulting file
pos1 <- regexpr("20", filename)[1]   # search for the first place "20" occurs inside filename
pos2 <- pos1 + 9
file_date <- substr(filename, pos1, pos2)    # pick out the part of the text from character no. 17 to no. 26
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
## - changed STATION_CODE for 1361 rows of data 
## 
## Fixing station 36A and 36A1 (36A1 shall count as 36A) 
## - changed STATION_CODE for 1367 rows of data 
## 
## Fix VDSI and Intersex at station 71G (both shall count as PARAM = 'VDSI/Intersex') 
## - changed PARAM for 12 rows of data
```

### Check: C/N lacking for 2019  

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
## is.na(VALUE_WW) 2015 2016 2017 2018 2019
##           FALSE  309  298  359  341  305
```

```r
data_all %>%
  filter(PARAM %in% "C/N" & MYEAR >= 2000) %>%
  xtabs(~is.na(VALUE_WW) + MYEAR, .)
```

```
##                MYEAR
## is.na(VALUE_WW) 2012 2013 2014 2015 2016 2017 2018 2019
##           FALSE  238  249  272  309  298  359  341  305
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
## Time difference of 17.52247 secs
```


### Calculate N, Det_limit and Over_LOQ

```r
t0 <- Sys.time()

data_med_1b <- data_all %>%
  group_by(MYEAR, STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM, UNIT) %>%
  summarise(
    N = n(), 
    Det_limit = median(VALUE_WW[!is.na(FLAG1)]), 
    Over_LOQ = sum(is.na(FLAG1)),
    .groups = "drop") %>%
  as_tibble()

Sys.time() - t0  # 4 secs
```

```
## Time difference of 3.333287 secs
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
    data_med_1b %>% select(N, Det_limit, Over_LOQ)
  )
} else {
  cat("data_med_1a and data_med_1b have different number of rows! Must be fixed, or use left_join(). \n")
}
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

### Add PROREF data to medians ----

```r
data_med_2 <- data_med_1 %>%
  pivot_longer(
    cols = c(VALUE_WW, VALUE_DW, VALUE_FB, VALUE_WWa, VALUE_DWa, VALUE_FBa), 
    names_to = "Basis", 
    values_to = "Value") %>%
  mutate(Basis = stringr::str_sub(Basis, start = 7))

data_med_3 <- data_med_2 %>%
  left_join(proref_updated03 %>% select(-N), by = c("LATIN_NAME", "TISSUE_NAME", "PARAM", "Basis"))

nrow(data_med_2)  # 525960
```

```
## [1] 525966
```

```r
nrow(data_med_3)  # 525960
```

```
## [1] 525966
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
## 3672
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
## 3672
```


## 5. Save  

```r
filename <- paste0("Data/110_mediandata_updated_", file_date, ".rds")

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
##   Data/110_mediandata_updated_2020-08-05.rds
```




