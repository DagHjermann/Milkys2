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
last_year <- 2019
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

files <- list_files("Data", pattern = "109_adjusted_data")
## There are 5 files with pattern '109_adjusted_data' to choose from
data_list <- read_rds_file(folder = "Data", 
                          files, 
                          filenumber = 1, 
                          get_date = TRUE,
                          time_since_modified = TRUE)
## File '109_adjusted_data_2020-08-05.rds' (file number 1) has been read 
##   This is the newest file. If you want to read an older file, put a different 'filenumber' 
## 
## Time since this file was modified: 
## Time difference of 4.277301 days

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
## - changed STATION_CODE for 1361 rows of data 
## 
## Fixing station 36A and 36A1 (36A1 shall count as 36A) 
## - changed STATION_CODE for 1367 rows of data 
## 
## Fix VDSI and Intersex at station 71G (both shall count as PARAM = 'VDSI/Intersex') 
## - changed PARAM for 12 rows of data
```

### Labware data     
Uses the most recent data (by default)    

```r
# Penultimate year:
files <- list_files(
  folder = "Input_data",
  pattern = paste0("Labware_samples_", last_year-1))
df_samples_labware_raw_previous <- read_rds_file(
  folder = "Input_data",
  files, 
  filenumber=1)

# Ultimate year:
cat("\n")
files <- list_files(
  folder = "Input_data",
  pattern = paste0("Labware_samples_", last_year))
df_samples_labware_raw_last <- read_rds_file(
  folder = "Input_data",
  files, 
  filenumber=1)

# Combine
df_samples_labware_raw <- bind_rows(
  df_samples_labware_raw_previous %>% mutate(MYEAR = last_year-1),
  df_samples_labware_raw_last %>% mutate(MYEAR = last_year)
)

cat("\n")
cat("Combined labware file - number of lines per year: \n")
xtabs(~MYEAR, df_samples_labware_raw)
```

```
## There are 1 files with pattern 'Labware_samples_2018' to choose from 
## File 'Labware_samples_2018_2020-09-11.rds' (file number 1) has been read 
## 
## There are 2 files with pattern 'Labware_samples_2019' to choose from 
## File 'Labware_samples_2019_2020-09-11.rds' (file number 1) has been read 
##   This is the newest file. If you want to read an older file, put a different 'filenumber' 
## 
## Combined labware file - number of lines per year: 
## MYEAR
## 2018 2019 
##  659  655
```


### Fix BIOTA_SAMPLENO = 0    
BIOTA_SAMPLENO corresponds to SAMPLE_NO2 in the data (i.e., sample number within station and tissue)    

```r
# xtabs(~BIOTA_SAMPLENO, df_samples_labware_raw)

sel <- df_samples_labware_raw$BIOTA_SAMPLENO == 0
cat(sum(sel), "samples have BIOTA_SAMPLENO == 0 \n")

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

# for checking the result
if (FALSE){
  # The clipped descriptions
  description_clipped[sel]
  # Extract number characters
  description_clipped[sel] %>%
    stringr::str_extract("[0-9]+")
}

# Extract number characters
df_samples_labware_raw$BIOTA_SAMPLENO[sel] <- description_clipped[sel] %>%
  stringr::str_extract("[0-9]+") %>%
  as.numeric()

cat("\n")
cat("Values of BIOTA_SAMPLENO after fix (should no zeros or NAs): \n")
xtabs(~addNA(BIOTA_SAMPLENO), df_samples_labware_raw)
```

```
## 60 samples have BIOTA_SAMPLENO == 0 
## 
## Tissue of samples with BIOTA_SAMPLENO == 0: 
## TISSUE
## BI-Galle 
##       60 
## 
## Values of BIOTA_SAMPLENO after fix (should no zeros or NAs): 
## addNA(BIOTA_SAMPLENO)
##    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16 
##  158  139  138   80   78   78   78   78   75   74   72   69   66   64   61    2 
##   17   18   19   23 <NA> 
##    1    1    1    1    0
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
    TISSUE_NAME = stringr::str_sub(TISSUE, start = 4),
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
## is.na(No_individuals) 2018 2019
##                 FALSE  659  655
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
  filter(MYEAR %in% c(last_year-1, last_year)) %>%
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
##  # A tibble: 124 x 5
##    MYEAR STATION_CODE LATIN_NAME     TISSUE_NAME     SAMPLE_NO2
##    <dbl> <chr>        <chr>          <chr>                <dbl>
##  1  2018 I969         Mytilus edulis Whole soft body          2
##  2  2018 I969         Mytilus edulis Whole soft body          3
##  3  2018 I969         Mytilus edulis Whole soft body          1
##  4  2018 I965         Mytilus edulis Whole soft body          3
##  5  2018 I965         Mytilus edulis Whole soft body          1
##  6  2018 I965         Mytilus edulis Whole soft body          2
##  7  2018 I964         Mytilus edulis Whole soft body          1
##  8  2018 I964         Mytilus edulis Whole soft body          2
##  9  2018 I964         Mytilus edulis Whole soft body          3
## 10  2018 30B          Gadus morhua   Lever                   11
## # … with 114 more rows
```

```r
xtabs(~is.na(No_individuals), data_all_samples)
xtabs(~is.na(No_individuals) + MYEAR, data_all_samples)  
```

```
## is.na(No_individuals)
## FALSE  TRUE 
## 52718    42 
##                      MYEAR
## is.na(No_individuals)  2018  2019
##                 FALSE 30203 22515
##                 TRUE     42     0
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
## STATION_CODE 2018
##          24B   21
##          30B   21
##             PARAM
## STATION_CODE DBALD DDC_ANT DDC_BBF DDC_DBF DDC_PA DDC_PS HCTBPH
##          24B     3       3       3       3      3      3      3
##          30B     3       3       3       3      3      3      3
```

### Make data set with N_string   
'dat_sample_string' - one line per station/tissue/parameter/year (same value for all Basis)   

```r
# dat_sample_string - contains 'N_string' which will be added to data by left-join 
#  - equals raw data summarised per station x parameter
dat_sample_string <- data_all_samples %>%
  filter(!is.na(VALUE_WW)) %>%
  group_by(MYEAR, STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM) %>%
  summarise(
    N = n(),
    N_pooled = sum(No_individuals > 1),
    Max_ind = max(No_individuals),
    .groups = "drop"
  ) %>%
  mutate(
    N_string = paste0(N, " (", N_pooled, "-", Max_ind, ")")  # Make string
  )

if (FALSE){
  View(dat_sample_string)
  
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
## 14 N_string contains 'NA'
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
  filter(MYEAR %in% c(last_year-1, last_year)) %>%
  select(MYEAR, STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM, SAMPLE_NO2,
         VALUE_WW, VALUE_DW, VALUE_FB, VALUE_WWa, VALUE_DWa, VALUE_FBa) %>%
  tidyr::pivot_longer(cols = c(VALUE_WW, VALUE_DW, VALUE_FB, VALUE_WWa, VALUE_DWa, VALUE_FBa), 
                      names_to = "Basis", values_to = "VALUE", values_drop_na = TRUE) %>%
  mutate(Basis = sub("VALUE_", "", Basis)) %>%
  group_by(MYEAR, STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM, Basis) %>%
  summarise(SD = sd(VALUE, na.rm = TRUE))
```

```
## `summarise()` regrouping output by 'MYEAR', 'STATION_CODE', 'LATIN_NAME', 'TISSUE_NAME', 'PARAM' (override with `.groups` argument)
```

```r
xtabs(~is.na(SD) + Basis + MYEAR, dat_sd)  
```

```
## , , MYEAR = 2018
## 
##          Basis
## is.na(SD)   DW  DWa   FB  FBa   WW  WWa
##     FALSE 4056  694 3865  649 4273  734
##     TRUE   173    0   11    0  173    0
## 
## , , MYEAR = 2019
## 
##          Basis
## is.na(SD)   DW  DWa   FB  FBa   WW  WWa
##     FALSE 2819  600 2724  559 3038  635
##     TRUE   169    0    9    0  170    0
```


## 5. D.d.i. (detectable data information)    
D.d.i. = detectable data information = "N>LOQ [min - maks]"    
Example:   
  7 [0.11 - 0.29] means 7 measurements over LOQ, and these measurements varied from 0.11 to 0.29  


```r
# Get LOQ flags (one row per station*parameter*sample)
data_all_flag <- data_all %>%
  filter(MYEAR %in% last_year) %>%
  select(STATION_CODE, LATIN_NAME, TISSUE_NAME, MYEAR, SAMPLE_NO2, UNIT, PARAM,
         FLAG1)

# Get values by making data set with one row per station*parameter*sample*basis
data_all_long <- data_all %>%
  filter(MYEAR %in% last_year) %>%
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
  group_by(STATION_CODE,LATIN_NAME,TISSUE_NAME,PARAM,Basis) %>%
  summarise(
    ddi_min = round(min(Value, na.rm = TRUE),4), 
    ddi_max = round(max(Value, na.rm = TRUE),4),
    .groups = "drop"
  ) %>%
  ungroup()

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
## # A tibble: 6 x 6
##   STATION_CODE LATIN_NAME   TISSUE_NAME PARAM Basis DDI                
##   <chr>        <chr>        <chr>       <chr> <chr> <chr>              
## 1 02B          Gadus morhua Lever       AG    DW    10 [0.1786-4.0541] 
## 2 02B          Gadus morhua Lever       AG    FB    10 [0.1818-3.6496] 
## 3 02B          Gadus morhua Lever       AG    WW    10 [0.1-1.5]       
## 4 02B          Gadus morhua Lever       AS    DW    11 [5.1786-16.9048]
## 5 02B          Gadus morhua Lever       AS    FB    11 [3.8208-18.883] 
## 6 02B          Gadus morhua Lever       AS    WW    11 [2.9-8.7]
```

## 6. Save  

```r
filename <- paste0("Data/111_Nstring_updated_", file_date, ".rds")
saveRDS(dat_sample_string, filename)
cat("Data of N_string saved as: \n")
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
##   Data/111_Nstring_updated_2020-08-05.rds 
## 
## Data of SD (standard deviation) saved as: 
##   Data/111_SD_updated_2020-08-05.rds 
## 
## Data of D.d.i. (detectable data information) saved as: 
##   Data/111_DDI_updated_2020-08-05.rds
```




