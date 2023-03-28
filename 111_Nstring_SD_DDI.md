---
title: "111_Nstring_and_SD"
output: 
  html_document:
    keep_md: true
    toc: true  
---

Creates information needed for the creation of the big Excel file:  
* 'N_string' for last year and current year     
    - "8 (6-3)" = 8 samples in total, 6 of them are pooled, max. individuals per pooled sample = 3    
* 'SD' (standard deviation) for last year and current year   
* 'D.d.i.' = detectable data information = "N>LOQ [min - maks]"    
    - Example: 7 [0.11 - 0.29] = 7 values >LOQ,  these measurements varied from 0.11 to 0.29   
  
As in script 110, some data series are 'homogenized'  

* *Input:* Uses the same input file as script 110, and in a way works in the same way (summarises sample-level data into one line per year/station/tissue/parameter)     
    - Uses labware data in addition   
* *Output* to three different files  
    - These are used by '201_Make_big_excel_file.Rmd'  

## 0. Current year  
The year of the last data


```r
selected_year <- 2021

if (as.numeric(substr(Sys.Date(), 1, 4)) - selected_year != 1){
  warning("NOTE: 'selected_year' should usually be last year. Is it set correctly?")
} else {
  message("OK ('selected_year' is one year ago)")
}
```

```
## OK ('selected_year' is one year ago)
```


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
library(safejoin)

source("002_Utility_functions.R")
source("110_Medians_and_PROREF_functions.R")   # homogenize_series
source("111_Nstring_SD_DDI_functions.R")
```


## 2. Read data

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
## Time difference of 21.52183 hours

data_all <- data_list$data
file_date <- data_list$file_date   
# 'file_date' will be used in part 10, when we save the resulting file
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

### Labware data     
Uses the most recent data (by default)     
* Made on the PC, project 'Milkys2_pc', using '801_Download_Labware_sample_data.Rmd'  

```r
# Penultimate year:
files <- list_files(
  folder = "Input_data",
  pattern = paste0("Labware_samples_", selected_year-1))
df_samples_labware_raw_previous <- read_rds_file(
  folder = "Input_data",
  files, 
  filenumber=1)

labware_yr <- lubridate::year(df_samples_labware_raw_previous$SAMPLED_DATE)
```

```
## Warning in system("timedatectl", intern = TRUE): running command 'timedatectl'
## had status 1
```

```r
labware_yr_same <- mean(labware_yr == (selected_year-1), na.rm = TRUE)
cat(100*labware_yr_same, "% of Labware years are in the expected year\n")
if (labware_yr_same < 0.75){
  stop("Too few samples seems to be from the expected year")
}

# Ultimate year:
cat("\n")
files <- list_files(
  folder = "Input_data",
  pattern = paste0("Labware_samples_", selected_year))
df_samples_labware_raw_last <- read_rds_file(
  folder = "Input_data",
  files, 
  filenumber=1)

labware_yr <- lubridate::year(df_samples_labware_raw_last$SAMPLED_DATE)
labware_yr_same <- mean(labware_yr == selected_year, na.rm = TRUE)
cat(100*labware_yr_same, "% of Labware years are in the expected year\n")  

if (labware_yr_same < 0.75){
  stop("Too few samples seems to be from the expected year")
}

# Combine
df_samples_labware_raw <- bind_rows(
  df_samples_labware_raw_previous %>% mutate(MYEAR = selected_year-1),
  df_samples_labware_raw_last %>% mutate(MYEAR = selected_year)
)

cat("\n")
cat("Combined labware file - number of lines per year: \n")
xtabs(~MYEAR, df_samples_labware_raw)
```

```
## There are 1 files with pattern 'Labware_samples_2020' to choose from 
## File 'Labware_samples_2020_2021-07-02.rds' (file number 1) has been read 
## 100 % of Labware years are in the expected year
## 
## There are 1 files with pattern 'Labware_samples_2021' to choose from 
## File 'Labware_samples_2021_2022-09-21.rds' (file number 1) has been read 
## 99.48231 % of Labware years are in the expected year
## 
## Combined labware file - number of lines per year: 
## MYEAR
## 2020 2021 
##  670 2446
```


### Fix BIOTA_SAMPLENO = 0    
BIOTA_SAMPLENO corresponds to SAMPLE_NO2 in the data (i.e., sample number within station and tissue)    

```r
# xtabs(~BIOTA_SAMPLENO, df_samples_labware_raw)

sel <- df_samples_labware_raw$BIOTA_SAMPLENO == 0
cat(sum(sel), "samples have BIOTA_SAMPLENO == 0 \n")

if (sum(sel) > 0){
  
  # All are BI-Galle
  cat("\nTissue of samples with BIOTA_SAMPLENO == 0: \n")
  xtabs(~TISSUE, df_samples_labware_raw[sel,])
  
  # Exctract sample from DESCRIPTION
  
  # First, remove AQUAMONITOR_CODE part from the DESCRIPTION strings
  description_clipped <- df_samples_labware_raw$DESCRIPTION
  line_numbers <- which(sel)
  
  # Loop through each line and remove AQUAMONITOR_CODE part
  for (i in line_numbers){
    description_clipped[i] <- sub(
      df_samples_labware_raw$AQUAMONITOR_CODE[i], 
      replacement = "",
      x = description_clipped[i])
  }
  
  extract_first_match <- function(string, pattern){
    m <- gregexpr(pattern, string)
    matches <- regmatches(string, m)
    sapply(matches, tail, n = 1)
    # tail(matches, 1)
  }
  # Test (vectorised)
  # extract_first_match(c("36B Færder area - torsk lever 7 og 11", "36B Færder area - torsk muskel 5"), "[0-9]+") 
  
  # for checking the result
  if (FALSE){
    # The clipped descriptions
    description_clipped[sel]
    # Extract number characters
    description_clipped[sel] %>%
      extract_first_match("[0-9]+")
  }
  
  # Extract number characters
  df_samples_labware_raw$BIOTA_SAMPLENO[sel] <- description_clipped[sel] %>%
    extract_first_match("[0-9]+") %>%
    as.numeric()
  
  cat("\n")
  cat("Values of BIOTA_SAMPLENO after fix (should no zeros or NAs): \n")
  xtabs(~addNA(BIOTA_SAMPLENO), df_samples_labware_raw)
  
}
```

```
## 0 samples have BIOTA_SAMPLENO == 0
```


## 3. N_string (sample size last year)   
Example: N_string "8 (6-3)" means that:   
- number of samples (pooled or unpooled) = 8   
- number of pooled samples = 6  
- maximum number of individuals per pooled sample = 3   

### Get number of individuals per pooled sample  
Uses function 'number_of_ind()' to get number of individuals per pooled sample   

```r
# Get number of individuals per pooled sample
# Use df_samples (table from LABWARE) 
# NOTE: Differs from code in 201 by extracting MYEAR and using it in grouping/summarising  
df_samples_labware <- df_samples_labware_raw %>%
  rename(STATION_CODE = AQUAMONITOR_CODE,
         LATIN_NAME = SPECIES,
         SAMPLE_NO2 = BIOTA_SAMPLENO
  ) %>%
  mutate(
    TISSUE_NAME = substr(TISSUE, start = 4, stop = 100),
    No_individuals = number_of_ind(X_BULK_BIO)
  ) %>%    # get number of individuals per pooled sample
  select(MYEAR, STATION_CODE, LATIN_NAME, TISSUE_NAME, SAMPLE_NO2, X_BULK_BIO, No_individuals) %>%
  group_by(MYEAR, STATION_CODE, LATIN_NAME, TISSUE_NAME, SAMPLE_NO2) %>%
  summarise_all(first) %>%
  ungroup() %>%
  # Special case for molluscs, which have no X_BULK_BIO value:
  mutate(No_individuals = case_when(
    LATIN_NAME %in% "Mytilus edulis" ~ 50,
    LATIN_NAME %in% c("Littorina littorea", "Nucella lapillus") ~ 30,
    is.na(No_individuals) ~ 1,     # in the case of fish, no X_BULK_BIO value means 1 individual
    TRUE ~ No_individuals)
  )

xtabs(~is.na(No_individuals) + MYEAR, df_samples_labware)
```

```
##                      MYEAR
## is.na(No_individuals) 2020 2021
##                 FALSE  669  927
```

### Fixing STATION_CODE     
Same as in above (and in script 110) 

```r
# Fixing station 227G1 and 227G2 (shall count as 227G) 
sel <- df_samples_labware$STATION_CODE %in% c("227G1","227G2")
# xtabs(~MYEAR, data_all[sel,])
df_samples_labware$STATION_CODE[sel] <- "227G"
cat("Fixing station 227G1 and 227G2 (shall count as 227G) \n")
cat("- STATION_CODE changed for", sum(sel), "rows \n")


# Fixing station 36A and 36A1 (36A1 shall count as 36A) 
sel <- df_samples_labware$STATION_CODE %in% "36A1"
# xtabs(~MYEAR, data_all[sel,])
df_samples_labware$STATION_CODE[sel] <- "36A"
cat("Fixing station 36A and 36A1 (36A1 shall count as 36A) \n")
cat("- STATION_CODE changed for", sum(sel), "rows \n")
```

```
## Fixing station 227G1 and 227G2 (shall count as 227G) 
## - STATION_CODE changed for 2 rows 
## Fixing station 36A and 36A1 (36A1 shall count as 36A) 
## - STATION_CODE changed for 6 rows
```

### Raw data with 'No_individuals' (info on pooled sample) added    

```r
data_all_samples <- data_all %>%
  filter(MYEAR %in% c(selected_year-1, selected_year)) %>%
  safe_left_join(subset(df_samples_labware, select = -X_BULK_BIO),
                 by = c("MYEAR", "STATION_CODE", "LATIN_NAME", "TISSUE_NAME", "SAMPLE_NO2"),
                 na_matches = "never",
                 check = "BCVm") %>%
  mutate(No_individuals = case_when(
    PARAM %in% c("ALAD", "EROD", "D4", "D5", "D6") ~ 1,  # these parameters are from single cod
    LATIN_NAME %in% "Mytilus edulis" ~ 50,    # industry blue mussel stations I964, I965, I969
    TRUE ~ No_individuals)
  )
```

```
## Warning: x has unmatched sets of joining values: 
##  # A tibble: 229 × 5
##    MYEAR STATION_CODE LATIN_NAME                TISSUE_NAME       SAMPLE_NO2
##    <dbl> <chr>        <chr>                     <chr>                  <dbl>
##  1  2020 I969         Mytilus edulis            Whole soft body            1
##  2  2020 I969         Mytilus edulis            Whole soft body            3
##  3  2020 I969         Mytilus edulis            Whole soft body            2
##  4  2020 I965         Mytilus edulis            Whole soft body            3
##  5  2020 I965         Mytilus edulis            Whole soft body            2
##  6  2020 I965         Mytilus edulis            Whole soft body            1
##  7  2020 71G          N. lapillus / L. littorea Whole soft body            1
##  8  2020 30B          Gadus morhua              Blod                       1
##  9  2020 30B          Gadus morhua              Liver - microsome          1
## 10  2020 30B          Gadus morhua              Blod                       2
## # … with 219 more rows
```

```r
xtabs(~is.na(No_individuals), data_all_samples)
xtabs(~is.na(No_individuals) + MYEAR, data_all_samples)  
```

```
## is.na(No_individuals)
## FALSE  TRUE 
## 50348   114 
##                      MYEAR
## is.na(No_individuals)  2020  2021
##                 FALSE 26853 23495
##                 TRUE    108     6
```

### Check data with 'No_individuals' (info on pooled sample) added   
Only left without are some very pretty strange parameters in cod liver in two stations   

```r
data_all_samples %>%
  filter(is.na(No_individuals)) %>%
  xtabs(~STATION_CODE + MYEAR, .)

data_all_samples %>%
  filter(is.na(No_individuals) & LATIN_NAME == "Gadus morhua") %>%
  xtabs(~STATION_CODE + PARAM, .)

if (FALSE){
  data_all_samples %>%
    filter(is.na(No_individuals)) %>%
    View()
}
```

```
##             MYEAR
## STATION_CODE 2020 2021
##         23B    47    0
##         30B    30    0
##         45B2    0    5
##         53B    30    0
##         71G     1    1
##             PARAM
## STATION_CODE % C % N AY380 BAP3OH C/N Delta13C Delta15N PA1OH PROTV PYR1OH
##         23B    0   0    11      1   0        0        0     1    33      1
##         30B    0   0     0      0   0        0        0     0    30      0
##         45B2   1   1     0      0   1        1        1     0     0      0
##         53B    0   0     0      0   0        0        0     0    30      0
```

### Make N_string data set, per parameter     
- 'dat_sample_string' - one line per station/tissue/parameter/year (same value for all Basis)   
- used in the big excel file  

```r
# dat_sample_string - contains 'N_string' which will be added to data by left-join 
#  - equals raw data summarised per station x parameter
dat_sample_string_temporary <- data_all_samples %>%
  # Don't include siloxans (D4-D6) because some eider duck have exra samples for them
  filter(!is.na(VALUE_WW) & !PARAM %in% c("D4","D5","D6")) %>%
  group_by(MYEAR, STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM) %>%
  summarise(
    N = n(),
    N_pooled = sum(No_individuals > 1),
    Max_ind = max(No_individuals),
    .groups = "drop"
  ) %>% # str()
  # Some special cases
  mutate(
    N_pooled = case_when(
      TISSUE_NAME %in% c("Galle", "Liver - microsome") ~ as.integer(0),
      STATION_CODE %in% "71G" ~ as.integer(1),
      STATION_CODE %in% "33F" ~ as.integer(3),
      TRUE ~ N_pooled),
    Max_ind = case_when(
      TISSUE_NAME %in% c("Galle", "Liver - microsome") ~ 1,
      STATION_CODE %in% "71G" ~ 60,
      STATION_CODE %in% "33F" ~ 5,
      TRUE ~ Max_ind)
  )

dat_sample_string <- dat_sample_string_temporary %>%
  mutate(
    N_string = paste0(N, " (", N_pooled, "-", Max_ind, ")")  # Make string
  )

if (FALSE){
  View(dat_sample_string)
  dat_sample_string %>% 
    filter(MYEAR == 2021 & STATION_CODE == "45B2") %>% 
    View("45B2")
  
  data_all_samples %>% 
    filter(MYEAR == 2021 & STATION_CODE == "45B2") %>% 
    View("45B2 all")
  
  sel <- grepl("NA", dat_sample_string$N_string)
  View(dat_sample_string[sel,])
  
  table(is.na(dat_sample_string$N_string))
  table(substr(addNA(dat_sample_string$N_string), 1, 1))
  table(dat_sample_string$N_string)
}

sel <- grepl("NA", dat_sample_string$N_string)
cat(sum(sel), "N_string contains 'NA' \n")
```

```
## 8 N_string contains 'NA'
```

### Make N_string data set, per tissue     
- 'dat_sample_string_tissue' - as 'dat_sample_string', but not one line per parameter  
- using max value 
- used for tables in report (made by ELU)  

```r
# dat_sample_string - contains 'N_string' which will be added to data by left-join 
#  - equals raw data summarised per station x parameter
dat_sample_string_tissue <- dat_sample_string_temporary %>%
  group_by(MYEAR, STATION_CODE, LATIN_NAME, TISSUE_NAME) %>%
  summarise(
    N = max(N, na.rm = TRUE),
    N_pooled = max(N_pooled, na.rm = TRUE),
    Max_ind = max(Max_ind),
    .groups = "drop"
  ) %>%
  mutate(
    N_string = paste0(N, " (", N_pooled, "-", Max_ind, ")")  # Make string
  ) %>%
  filter(LATIN_NAME != "N. lapillus / L. littorea")

sel <- grepl("NA", dat_sample_string_tissue$N_string)
cat(sum(sel), "N_string contains 'NA' \n")
```

```
## 4 N_string contains 'NA'
```

### Imposex and intersex - TO INCLUDE LATER?

```r
#
# Imposex and intersex - TO INCLUDE LATER   
#
if (FALSE){
  data_imposex <- readRDS("Data/32_data_imposex.rds")
  
  df_numbers_imposex <- data_imposex %>%
    filter(Sex %in% "f" & PARAM %in% c("VDSI","Intersex") & !is.na(VALUE_WW)) %>%
    group_by(STATION_CODE) %>%
    summarise(Max_ind = n()) %>%
    ungroup() %>%
    mutate(
      N_string_new = paste0(1, " (", 1, "-", Max_ind, ")")
    ) %>%
    select(STATION_CODE, N_string_new)
  
  #
  # Replace N_string values for imposex and intersex 
  #
  dat_sample_string <- dat_sample_string %>%
    safe_left_join(df_numbers_imposex, 
                   by = c("STATION_CODE"),
                   na_matches = "never",
                   check = "BCV") %>%
    mutate(N_string = ifelse(is.na(N_string_new), N_string, N_string_new)) %>%
    select(-N_string_new)
}
# end of "Imposex and intersex" part
```


## 4. Standard deviation (SD)      
* 'dat_all_sd' - one line per station/tissue/parameter/year/basis (different values for different Basis)  
* No values for VDSI because they are given by a single value   

```r
dat_sd <- data_all %>%
  filter(MYEAR %in% c(selected_year-1, selected_year)) %>%
  select(MYEAR, STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM, SAMPLE_NO2,
         VALUE_WW, VALUE_DW, VALUE_FB, VALUE_WWa, VALUE_DWa, VALUE_FBa) %>%
  tidyr::pivot_longer(cols = c(VALUE_WW, VALUE_DW, VALUE_FB, VALUE_WWa, VALUE_DWa, VALUE_FBa), 
                      names_to = "Basis", values_to = "VALUE", values_drop_na = TRUE) %>%
  mutate(Basis = sub("VALUE_", "", Basis)) %>%
  group_by(MYEAR, STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM, Basis) %>%
  summarise(SD = sd(VALUE, na.rm = TRUE))
```

```
## `summarise()` has grouped output by 'MYEAR', 'STATION_CODE', 'LATIN_NAME',
## 'TISSUE_NAME', 'PARAM'. You can override using the `.groups` argument.
```

```r
xtabs(~is.na(SD) + Basis + MYEAR, dat_sd)  
```

```
## , , MYEAR = 2020
## 
##          Basis
## is.na(SD)   DW  DWa   FB  FBa   WW  WWa
##     FALSE 3132 1049 3089 1021 3387 1130
##     TRUE   334    0  190    0  334    0
## 
## , , MYEAR = 2021
## 
##          Basis
## is.na(SD)   DW  DWa   FB  FBa   WW  WWa
##     FALSE 1371   24 2563  902 2758  982
##     TRUE   257    0   99    3  258    3
```


## 5. D.d.i. (detectable data information)    
D.d.i. = detectable data information = "N>LOQ [min - maks]"    
Example:   
  7 [0.11 - 0.29] means 7 measurements over LOQ, and these measurements varied from 0.11 to 0.29  


```r
# Get LOQ flags (one row per station*parameter*sample)
data_all_flag <- data_all %>%
  filter(MYEAR %in% selected_year) %>%
  select(STATION_CODE, LATIN_NAME, TISSUE_NAME, MYEAR, SAMPLE_NO2, UNIT, PARAM,
         FLAG1)
  
# Get values by making data set with one row per station*parameter*sample*basis
data_all_long <- data_all %>%
  filter(MYEAR %in% selected_year) %>%
  select(STATION_CODE, LATIN_NAME, TISSUE_NAME, MYEAR, SAMPLE_NO2, UNIT, PARAM,
         VALUE_WW, VALUE_DW, VALUE_FB, VALUE_WWa, VALUE_DWa, VALUE_FBa) %>%
  tidyr::pivot_longer(cols = starts_with("VALUE"), 
                      names_to = "Basis", values_to = "Value", values_drop_na = TRUE) %>%
  mutate(Basis = sub("VALUE_", "", Basis)) %>%
  # Add LOQ flags to values (creating one row per parameter*sample*basis)  
  safe_left_join(
    data_all_flag,
    by = c("STATION_CODE", "LATIN_NAME", "TISSUE_NAME", "MYEAR", 
                        "SAMPLE_NO2", "UNIT", "PARAM"),
    na_matches = "never",
    check = "BCV")

# Get min & max values, for adding to df_ddi later (one row per parameter*basis)
df_ddi_minmax <- data_all_long %>%
  filter(is.na(FLAG1)) %>%
  # Appropriate rounding of values:
  mutate(
    Value = case_when(
      Value < 0.001 ~ round(Value, 4),
      Value < 0.01 ~ round(Value, 3),
      Value < 2 ~ round(Value, 2),
      Value < 20 ~ round(Value, 1),
      TRUE ~ round(Value, 0)
    )) %>%
  group_by(STATION_CODE,LATIN_NAME,TISSUE_NAME,PARAM,Basis) %>%
  summarise(
    ddi_min = min(Value, na.rm = TRUE), 
    ddi_max = max(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ungroup()

# If you get an error with the following message:
#   "Error: y is not unique on STATION_CODE, LATIN_NAME, TISSUE_NAME, MYEAR, SAMPLE_NO2, UNIT and PARAM "
# run the following
# check <- data_all_flag %>%
#   add_count(STATION_CODE, LATIN_NAME, TISSUE_NAME, MYEAR, SAMPLE_NO2, UNIT, PARAM) %>%
#   filter(n > 1)

#
# Create D.d.i
#
# Start DDI data set (df_ddi) by counting values over LOQ  #
dat_ddi <- data_all_long %>%
  group_by(STATION_CODE,LATIN_NAME,TISSUE_NAME,PARAM,Basis) %>%
  summarise(
    Over_LOQ = sum(is.na(FLAG1)),  .groups = "drop"
  ) %>%
  # Add min and max values
  safe_left_join(
    df_ddi_minmax, 
    by = c("STATION_CODE", "LATIN_NAME", "TISSUE_NAME", "PARAM", "Basis"),
    na_matches = "never",
    check = "BCV") %>%
  # Create the D.d.i. string
  mutate(DDI = case_when(
    Over_LOQ > 1 & is.finite(ddi_min) & !is.na(ddi_min) ~ paste0(Over_LOQ, " [", ddi_min, "-", ddi_max, "]"),
    Over_LOQ == 1 & is.finite(ddi_min) & !is.na(ddi_min) ~ paste0(Over_LOQ, " [", ddi_min, "]"),
    Over_LOQ == 0 ~ paste0(Over_LOQ, " [n.a.]")
  )
  )

dat_ddi %>%
  select(STATION_CODE,LATIN_NAME,TISSUE_NAME,PARAM,Basis,DDI) %>%
  head()
```

```
## # A tibble: 6 × 6
##   STATION_CODE LATIN_NAME   TISSUE_NAME PARAM Basis DDI          
##   <chr>        <chr>        <chr>       <chr> <chr> <chr>        
## 1 02B          Gadus morhua Lever       AG    FB    5 [0.82-12.1]
## 2 02B          Gadus morhua Lever       AG    FBa   5 [0.87-13.5]
## 3 02B          Gadus morhua Lever       AG    WW    5 [0.16-0.58]
## 4 02B          Gadus morhua Lever       AG    WWa   5 [0.15-0.52]
## 5 02B          Gadus morhua Lever       AS    FB    5 [5.4-258]  
## 6 02B          Gadus morhua Lever       AS    FBa   5 [6.6-297]
```

## 6. Save  

```r
filename <- paste0("Data/111_Nstring_updated_", file_date, ".rds")
saveRDS(dat_sample_string, filename)
cat("Data of N_string saved as: \n")
cat(" ", filename, "\n")

cat("\n")
filename <- paste0("Data/111_Nstring_tissue_updated_", file_date, ".xlsx")
writexl::write_xlsx(dat_sample_string_tissue, filename)
cat("Data of N_string, tissue-wise, saved to Excel (for ELU) as: \n")
cat(" ", filename, "\n")

cat("\n")
filename <- paste0("Data/111_SD_updated_", file_date, ".rds")
saveRDS(dat_sd, filename)
cat("Data of SD (standard deviation) saved as: \n")
cat(" ", filename, "\n")

cat("\n")
filename <- paste0("Data/111_DDI_updated_", file_date, ".rds")
saveRDS(dat_ddi, filename)
cat("Data of D.d.i. (detectable data information) saved as: \n")
cat(" ", filename, "\n")
```

```
## Data of N_string saved as: 
##   Data/111_Nstring_updated_2022-09-23.rds 
## 
## Data of N_string, tissue-wise, saved to Excel (for ELU) as: 
##   Data/111_Nstring_tissue_updated_2022-09-23.xlsx 
## 
## Data of SD (standard deviation) saved as: 
##   Data/111_SD_updated_2022-09-23.rds 
## 
## Data of D.d.i. (detectable data information) saved as: 
##   Data/111_DDI_updated_2022-09-23.rds
```


```r
if (FALSE){
  
  dat_sample_string <- readRDS("Data/111_Nstring_updated_2021-09-15.rds")
  
}
```



