---
title: "03_Read_fish_individual_data"
author: "DHJ"
date: "31 8 2021"
output: 
  html_document:
    keep_md: true
---

## Reading fish data from excel files   

* Before starting on this, you need to manually copy the "fish files" to Jupyterhub  

    - Present (early 2022) location: 

## Libraries  


```r
library(dplyr)
library(tidyr)
library(purrr)     # map
library(readxl)
library(ggplot2)
library(stringr)
source("150_Get_fish_individual_data_functions.R")
```
## Year  

```r
selected_year <- 2021
```


## Files

```r
datafolder <- paste0("Input_data/opparbeiding_biota/Fisk_", selected_year)

# dir.exists(datafolder)

fns <- dir(datafolder, "*.xls*")
fns <- fns[substr(fns,1,1) != "~"]   # 'locked' file (remove opened excel files)
fns %>% dput()
```

```
## c("33F_sande_skrubbe_2021.xlsx", "x02B_Hvaler_2021.xlsx", "x10B_varanger2021.xlsx", 
## "x13B_Kristiansand_2021.xlsx", "x15B_Farsund_2021.xlsx", "x19B_Ytre_Isfjorden_2021.xlsx", 
## "x20B_Longyearbyen_2021.xlsx", "x23B_karihavet_2021.xlsx", "x24B_Bergen_2021.xlsx", 
## "x28B_Ålesund_2021.xlsx", "x30B_Indre_oslofjord2021_ny.xlsx", 
## "x36B_Færder_2021_torsk.xlsx", "x43B_Tromsø_2021.xlsx", "x45B2_Hammerfest_2021.xlsx", 
## "x53B_indre_sørfjorden_2021.xlsx", "x71B_Grenland_2021.xlsx", 
## "x80B_Munkholmen_2021.xlsx", "x96B_Sandnessjøen_2021.xlsx", 
## "x98B_lofoten2021.xlsx")
```

## Test read of one file  
Used to make the read_fish_individual_data() function. Using search_lineno()    

```r
fn <- fns[1]

df_try <- read_excel(paste0(datafolder, "/", fn))
```

```
## New names:
## • `` -> `...2`
## • `` -> `...3`
## • `` -> `...4`
## • `` -> `...5`
## • `` -> `...6`
## • `` -> `...7`
## • `` -> `...8`
## • `` -> `...9`
## • `` -> `...10`
## • `` -> `...11`
## • `` -> `...12`
## • `` -> `...13`
## • `` -> `...14`
## • `` -> `...15`
## • `` -> `...16`
## • `` -> `...17`
## • `` -> `...18`
## • `` -> `...19`
## • `` -> `...20`
## • `` -> `...21`
## • `` -> `...22`
## • `` -> `...23`
## • `` -> `...24`
```

```r
line_header <- search_lineno(df_try, 1, "Fisknr")
dat <- read_excel(paste0(datafolder, "/", fn), skip = line_header - 1)

# If file has a hole (empty row) in the data, we delete data from the first hole
first_empty <- which(is.na(dat$Fisknr))[1]
if (!is.na(first_empty))
  dat <- dat[1:(first_empty-1),]

dat
```

```
## # A tibble: 30 × 24
##    Fisknr `Lengde (cm)` `Vekt (g)` `Filetprøve (g)` `Levervekt (g)` `Leverprøve (g)` Leverfarge `Kjønn (M/F)` `Gonadevekt (g)` `M-kode` `otolitt antall` blod  galle
##    <chr>          <dbl>      <dbl> <chr>                      <dbl>            <dbl> <chr>      <chr>         <lgl>            <lgl>               <dbl> <lgl> <lgl>
##  1 1               32          416 30                           7.8              7.8 G          F             NA               NA                      2 NA    NA   
##  2 2               32.5        318 26.3                         1.5              1.5 G          F             NA               NA                      2 NA    NA   
##  3 3               33.5        486 32.9                         9.6              9.6 B          F             NA               NA                      2 NA    NA   
##  4 4               33.5        406 30.3                         6.9              6.9 G          F             NA               NA                      2 NA    NA   
##  5 5               34          507 31.2                         9.4              9.4 G          F             NA               NA                      2 NA    NA   
##  6 6               35          520 30.5                         9.9              9.9 G          F             NA               NA                      2 NA    NA   
##  7 7               35          498 30.5                        13.2             13.2 G          F             NA               NA                      2 NA    NA   
##  8 8               35          380 26                           5.7              5.7 G          F             NA               NA                      2 NA    NA   
##  9 9               35.5        390 26.6                         3.2              3.2 B          F             NA               NA                      2 NA    NA   
## 10 10              35.5        484 30.1                         6.9              6.9 G          F             NA               NA                      2 NA    NA   
## # … with 20 more rows, and 11 more variables: leverbit <lgl>, Kommentar <lgl>, alder <lgl>, `otolittform kyst/vandre` <lgl>, hudfarge <lgl>, `dato fangst` <chr>,
## #   `sted fangst` <lgl>, `del av blandprøvenr` <dbl>, `mage+tarm til plast` <lgl>, `Leverprøve kontollveid (ja /nei)` <lgl>, `2 g lever til siloksaner` <lgl>
```

## Test 'read_fish_individual_data'

```r
# debugonce(read_fish_individual_data )
read_fish_individual_data(fns[1])
```

```
## New names:
## • `` -> `...1`
## • `` -> `...2`
## • `` -> `...3`
## • `` -> `...4`
## • `` -> `...5`
## • `` -> `...6`
## • `` -> `...7`
## • `` -> `...8`
## • `` -> `...9`
## • `` -> `...10`
## • `` -> `...11`
## • `` -> `...12`
## • `` -> `...13`
## • `` -> `...14`
## • `` -> `...15`
## • `` -> `...16`
## • `` -> `...17`
## • `` -> `...18`
## • `` -> `...19`
## • `` -> `...20`
## • `` -> `...21`
## • `` -> `...22`
## • `` -> `...23`
## • `` -> `...24`
```

```
## # A tibble: 15 × 26
##    Station   Species Fisknr `Lengde (cm)` `Vekt (g)` `Filetprøve (g)`   `Levervekt (g)` `Leverprøve (g)` Leverfarge `Kjønn (M/F)` `Gonadevekt (g)` `M-kode`
##    <chr>     <chr>   <chr>          <dbl>      <dbl> <chr>                        <dbl>            <dbl> <chr>      <chr>         <lgl>            <lgl>   
##  1 33F Sande Skrubbe 1               32          416 30                             7.8              7.8 G          F             NA               NA      
##  2 33F Sande Skrubbe 2               32.5        318 26.3                           1.5              1.5 G          F             NA               NA      
##  3 33F Sande Skrubbe 3               33.5        486 32.9                           9.6              9.6 B          F             NA               NA      
##  4 33F Sande Skrubbe 4               33.5        406 30.3                           6.9              6.9 G          F             NA               NA      
##  5 33F Sande Skrubbe 5               34          507 31.2                           9.4              9.4 G          F             NA               NA      
##  6 33F Sande Skrubbe 6               35          520 30.5                           9.9              9.9 G          F             NA               NA      
##  7 33F Sande Skrubbe 7               35          498 30.5                          13.2             13.2 G          F             NA               NA      
##  8 33F Sande Skrubbe 8               35          380 26                             5.7              5.7 G          F             NA               NA      
##  9 33F Sande Skrubbe 9               35.5        390 26.6                           3.2              3.2 B          F             NA               NA      
## 10 33F Sande Skrubbe 10              35.5        484 30.1                           6.9              6.9 G          F             NA               NA      
## 11 33F Sande Skrubbe 11              36.5        445 20.100000000000001             3.1              3.1 B          F             NA               NA      
## 12 33F Sande Skrubbe 12              36.5        677 31                            16.9             10   G          F             NA               NA      
## 13 33F Sande Skrubbe 13              37.5        502 30.7                           5.5              5.5 GB         F             NA               NA      
## 14 33F Sande Skrubbe 14              38          672 31                            13.7              9.5 G          F             NA               NA      
## 15 33F Sande Skrubbe 15              38.5        659 30.7                           4.4              4.4 B          F             NA               NA      
## # … with 14 more variables: `otolitt antall` <dbl>, blod <lgl>, galle <lgl>, leverbit <lgl>, Kommentar <lgl>, alder <lgl>, `otolittform kyst/vandre` <lgl>,
## #   hudfarge <lgl>, `dato fangst` <chr>, `sted fangst` <lgl>, `del av blandprøvenr` <dbl>, `mage+tarm til plast` <lgl>, `Leverprøve kontollveid (ja /nei)` <lgl>,
## #   `2 g lever til siloksaner` <lgl>
```

## Read all data into a list of data frames  
    - Reads all columns of each file  
    - Each data frame may differ from the other   

### Extract station code to use for list names  

* should be identical to station code    


```r
fns_to_read <- fns

# Start by Extract all characters to the left of the first underscore  
names(fns_to_read) <- stringr::str_extract(fns, "[^_]+")

# Check
names(fns_to_read)
```

```
##  [1] "33F"   "x02B"  "x10B"  "x13B"  "x15B"  "x19B"  "x20B"  "x23B"  "x24B"  "x28B"  "x30B"  "x36B"  "x43B"  "x45B2" "x53B"  "x71B"  "x80B"  "x96B"  "x98B"
```

```r
# Remove first 'x'  
sel <- str_sub(names(fns_to_read), 1, 1) == "x"
names(fns_to_read)[sel] <- str_sub(names(fns_to_read)[sel], start = 2)

# Check
names(fns_to_read)
```

```
##  [1] "33F"  "02B"  "10B"  "13B"  "15B"  "19B"  "20B"  "23B"  "24B"  "28B"  "30B"  "36B"  "43B"  "45B2" "53B"  "71B"  "80B"  "96B"  "98B"
```

```r
# Manual change, if needed  
# names(fns_to_read)[grepl("10B", fns)] <- "10B"
# names(fns_to_read)[grepl("96B", fns)] <- "96B"
```

    
### Read data into list   

* Takes a couple of minutes  


```r
# Make "safe" version of 'read_fish_individual_data'
# Meaning that we continue with next file even if there is an error in one file 
read_fish_individual_data_s <- safely(read_fish_individual_data)

# Results in list with 17 items, where each item consistst of "$result" and "$error"
dat_list_safe <- fns_to_read %>% map(read_fish_individual_data_s)  

# Transpose list
# Results in list with 2 items ("$result" and "$error"), each with 17 items 
dat_list_safe <- purrr::transpose(dat_list_safe)

names(dat_list_safe$result)
```

```
##  [1] "33F"  "02B"  "10B"  "13B"  "15B"  "19B"  "20B"  "23B"  "24B"  "28B"  "30B"  "36B"  "43B"  "45B2" "53B"  "71B"  "80B"  "96B"  "98B"
```


### Check results  


```r
is_ok <- dat_list_safe$error %>% map_lgl(is.null)

if (sum(!is_ok) > 0){
  
  # If any files didn't work, you will get file number here
  which(!is_ok) 
  
  # If any files didn't work, you will get file name here
  fns_to_read[!is_ok] 
  
} else {
  
  message("All files were read")
  
}
```

```
## All files were read
```

```r
# If NOT ok, uncomment the next lines ('15' should be replaced by number of file which casued error ): 
# debugonce(read_fish_individual_data)
# read_fish_individual_data(fns_to_read[15])
```

### Resulting list of results

```r
dat_list <- dat_list_safe$result[is_ok]

# Get names as well:
names(dat_list) <- names(dat_list_safe$result)[is_ok]
```

## Check column names   

* Goes through column numer 1, 2 etc.   
* For each column, checks the name of that column in all tables    
* If it says 'Only value', it means that all tables have the same column name (and thus are ok)  


```r
# Number of columns per file:
# dat_list %>% map_int(length)    # always 26
  
# Get column name for a given column number 
get_colname <- function(df, colnumber) colnames(df)[colnumber]

# Get all column names, by column number 
for (i in 1:26){
  cat("Column", i, ":  \n")
  column_names <- dat_list %>% map_chr(get_colname, colnumber = i)
  print_less_common_values(column_names)
  cat("------------------------------------\n")
  }
```

```
## Column 1 :  
## Only value:
##   'Station' (19 values)
## ------------------------------------
## Column 2 :  
## Only value:
##   'Species' (19 values)
## ------------------------------------
## Column 3 :  
## Only value:
##   'Fisknr' (19 values)
## ------------------------------------
## Column 4 :  
## Only value:
##   'Lengde (cm)' (19 values)
## ------------------------------------
## Column 5 :  
## Only value:
##   'Vekt (g)' (19 values)
## ------------------------------------
## Column 6 :  
## Only value:
##   'Filetprøve (g)' (19 values)
## ------------------------------------
## Column 7 :  
## Only value:
##   'Levervekt (g)' (19 values)
## ------------------------------------
## Column 8 :  
## Only value:
##   'Leverprøve (g)' (19 values)
## ------------------------------------
## Column 9 :  
## Only value:
##   'Leverfarge' (19 values)
## ------------------------------------
## Column 10 :  
## Only value:
##   'Kjønn (M/F)' (19 values)
## ------------------------------------
## Column 11 :  
## Only value:
##   'Gonadevekt (g)' (19 values)
## ------------------------------------
## Column 12 :  
## Only value:
##   'M-kode' (19 values)
## ------------------------------------
## Column 13 :  
## Only value:
##   'otolitt antall' (19 values)
## ------------------------------------
## Column 14 :  
## Only value:
##   'blod' (19 values)
## ------------------------------------
## Column 15 :  
## Only value:
##   'galle' (19 values)
## ------------------------------------
## Column 16 :  
## Only value:
##   'leverbit' (19 values)
## ------------------------------------
## Column 17 :  
## Only value:
##   'Kommentar' (19 values)
## ------------------------------------
## Column 18 :  
## Only value:
##   'alder' (19 values)
## ------------------------------------
## Column 19 :  
## Only value:
##   'otolittform kyst/vandre' (19 values)
## ------------------------------------
## Column 20 :  
## Only value:
##   'hudfarge' (19 values)
## ------------------------------------
## Column 21 :  
## Only value:
##   'dato fangst' (19 values)
## ------------------------------------
## Column 22 :  
## Only value:
##   'sted fangst' (19 values)
## ------------------------------------
## Column 23 :  
## Only value:
##   'del av blandprøvenr' (19 values)
## ------------------------------------
## Column 24 :  
## Only value:
##   'mage+tarm til plast' (19 values)
## ------------------------------------
## Column 25 :  
## Only value:
##   'Leverprøve kontollveid (ja /nei)' (19 values)
## ------------------------------------
## Column 26 :  
## Only value:
##   '2 g lever til siloksaner' (19 values)
## ------------------------------------
```

## Print all columns  
Change 'print_all' to TRUE to show result

```r
print_all <- FALSE

if (print_all){
  for (i in 1:26){
  cat(i, "\n")
  print(
    dat_list %>% map_chr(get_colname, colnumber = i)
  )
  cat("------------------------------------\n")
  }

}
```

## Pick specific columns from each data frame   

Levervekt not found for data set 3 (13B)  
Note: Upper- and lowercase is ignored - no need to worry about this   
- e.g.: 'Vekt' will find both 'Vekt(g)' and 'vekt(g)'


```r
# Get selected columns  
# 
dat_list_selected <- dat_list %>% 
  map(get_columns, colnames = c("Station", "Species", "Fisknr", "Lengde", "Vekt", "Kjønn", 
                                "Levervekt", "Leverfarge", "Gonadevekt", "Alder"))
```

```
## All variables found
## All variables found
## All variables found
## All variables found
## All variables found
## All variables found
## All variables found
## All variables found
## All variables found
## All variables found
## All variables found
## All variables found
## All variables found
## All variables found
## All variables found
## All variables found
## All variables found
## All variables found
## All variables found
```

```r
# Look for Levervekt:
# dat_list[[3]]
```

## Make data frame list ready for combining, part 1  

```r
# Fix station codes using names of the list   
for (i in seq_along(dat_list_selected)){
  dat_list_selected[[i]]$Station <- names(dat_list_selected)[i]
}

#
# Check Station
#
cat("Existing values of in data:\n")
```

```
## Existing values of in data:
```

```r
dat_list_selected %>% map_chr(~unique(.$Station))  
```

```
##    33F    02B    10B    13B    15B    19B    20B    23B    24B    28B    30B    36B    43B   45B2    53B    71B    80B    96B    98B 
##  "33F"  "02B"  "10B"  "13B"  "15B"  "19B"  "20B"  "23B"  "24B"  "28B"  "30B"  "36B"  "43B" "45B2"  "53B"  "71B"  "80B"  "96B"  "98B"
```

```r
#
# Check Species (NOTE: HARD-CODED FILE NUMBERS)
#
dat_list_selected %>% map_chr(~unique(.$Species))
```

```
##       33F       02B       10B       13B       15B       19B       20B       23B       24B       28B       30B       36B       43B      45B2       53B       71B 
## "Skrubbe"        NA   "torsk"   "TORSK"   "Torsk"   "Torsk"   "Torsk"   "Torsk"   "Torsk"   "TORSK"   "Torsk"   "torsk"   "Torsk"   "Torsk"   "Torsk"   "TORSK" 
##       80B       96B       98B 
##   "Torsk"   "Torsk"   "torsk"
```

## Make data frame list ready for combining, part 2    

### Check classes of each column   

* Check up those that have unexcpected/different classes, e.g. character when it should have been numeric  


```r
dat_list_selected %>% map_df(get_column_classes)
```

```
##      Station   Species    Fisknr    Lengde      Vekt     Kjønn Levervekt Leverfarge Gonadevekt   Alder
## 1  character character character   numeric   numeric character   numeric  character    logical logical
## 2  character character character   numeric   numeric character character  character    logical numeric
## 3  character character character character   numeric character   numeric  character    logical numeric
## 4  character character character   numeric   numeric character   numeric  character    numeric numeric
## 5  character character character   numeric character character   numeric  character  character numeric
## 6  character character character   numeric   numeric character   numeric  character    numeric numeric
## 7  character character character   numeric   numeric character   numeric  character    numeric numeric
## 8  character character character   numeric   numeric character   numeric  character    numeric numeric
## 9  character character character character   numeric character   numeric  character    logical numeric
## 10 character character character   numeric   numeric character   numeric  character    logical numeric
## 11 character character character   numeric   numeric character   numeric  character  character numeric
## 12 character character character   numeric   numeric character   numeric  character    logical numeric
## 13 character character character   numeric   numeric character   numeric  character    numeric numeric
## 14 character character character character   numeric character   numeric  character    logical numeric
## 15 character character character   numeric   numeric character   numeric  character    numeric logical
## 16 character character character   numeric   numeric character   numeric  character    logical numeric
## 17 character character character   numeric   numeric character   numeric  character    logical numeric
## 18 character character character   numeric   numeric character   numeric  character    logical numeric
## 19 character character character   numeric   numeric character   numeric  character    numeric numeric
```
### Manual check of some columns    

* char2numeric handles decimal comma instead of decimal dot, but not other changes  
* So just check in case    


```r
if (FALSE){
  
  dat_list_selected[[3]]$Lengde
  dat_list_selected[[9]]$Lengde
  dat_list_selected[[14]]$Lengde
  dat_list_selected[[5]]$Vekt
  dat_list_selected[[2]]$Levervekt
  dat_list_selected[[1]]$Gonadevekt
  dat_list_selected[[5]]$Gonadevekt
  dat_list_selected[[11]]$Gonadevekt
  
}

# One value contained a '+', remoce it
dat_list_selected[[2]]$Levervekt <- sub("+", "",   dat_list_selected[[2]]$Levervekt, fixed = TRUE)
```

### Change columns that should be numeric, to be numeric   

* If it is character and contains decimal comma, that will be converted by 'char2numeric'     
* Things like "nd" will jsut get NA value (as they should) 
* All numeric columns are changed, to be on the safe side


```r
dat_list_selected <- dat_list_selected %>% map(~char2numeric(., "Fisknr"))
dat_list_selected <- dat_list_selected %>% map(~char2numeric(., "Lengde"))
dat_list_selected <- dat_list_selected %>% map(~char2numeric(., "Vekt"))
dat_list_selected <- dat_list_selected %>% map(~char2numeric(., "Levervekt"))
dat_list_selected <- dat_list_selected %>% map(~char2numeric(., "Gonadevekt"))
```

```
## Warning in char2numeric(., "Gonadevekt"): NAs introduced by coercion
```

```r
dat_list_selected <- dat_list_selected %>% map(~char2numeric(., "Alder"))
```

### Check lacking length  

```r
n_lacking <- dat_list_selected %>% map_int(~sum(is.na(.$Lengde)))

if (sum(n_lacking > 0) > 0) {
  stop(sum(n_lacking > 0), " station(s) have lacking 'Length' rows. Check them up! ")
} else {
  message("No stations have lacking 'Length' rows! All OK.")
}
```

```
## No stations have lacking 'Length' rows! All OK.
```

```r
# Only if needed!
if (FALSE){
  
  n_lacking[n_lacking > 0]
  
  
  # Check manually:
  # dat_list_selected[["15B"]] %>% View()
  
  # Fix (HARD-CODED):
  dat_list_selected[["15B"]] <- dat_list_selected[["15B"]][1:15,]
  
  # Check again:
  n_lacking <- dat_list_selected %>% map_int(~sum(is.na(.$Lengde)))
  if (sum(n_lacking > 0) > 0) {
    stop("Still ", sum(n_lacking > 0), " station(s) have lacking 'Length' rows. Check them up! ")
  } else {
    message("Fixed - no lacking 'Length' rows") 
  }
  
}
```


## Combine into a single data frame  

```r
dat_1 <- bind_rows(dat_list_selected, .id = "STATION_CODE")

# dat

nrow(dat_1)
```

```
## [1] 337
```

## Fix specific columns   

### Create STATION_CODE    
- STATION_CODE = name removed from "code + name" stations   
- NOT NEEDED anymore, did this through the file name

```r
# not needed, used '.id = "STATION_CODE"' in previous chunk

# xtabs(~addNA(Station), dat_1)
# 
# dat_2 <- dat_1 %>% 
#   mutate(STATION_CODE = stringr::str_extract(Station, "([^[[:blank:]]]+)") ) %>%
#   mutate(STATION_CODE = sub(",", "", STATION_CODE, fixed = TRUE)) %>%
#   mutate(STATION_CODE = sub("b", "B", STATION_CODE)) %>%
#   mutate(STATION_CODE = ifelse(STATION_CODE %in% "43B", "43B2", STATION_CODE))  # 43B2 is the present code
# 
# xtabs(~addNA(STATION_CODE), dat_2)

dat_2 <- dat_1
```

### Fix Species


```r
# Check   
cat("\nCheck 1 \n")
```

```
## 
## Check 1
```

```r
xtabs(~addNA(Species), dat_2)
```

```
## addNA(Species)
## Skrubbe   torsk   Torsk   TORSK    <NA> 
##      15      47     208      60       7
```

```r
dat_3 <- dat_2 %>% 
  mutate(Species = stringr::str_to_sentence(Species)) %>%   # 'torsk' => 'Torsk'
  mutate(Species = ifelse(grepl("Gadu", Species), "Torsk", Species))

# Check again  
cat("\nCheck 2 \n")
```

```
## 
## Check 2
```

```r
xtabs(~addNA(Species), dat_3)
```

```
## addNA(Species)
## Skrubbe   Torsk    <NA> 
##      15     315       7
```

```r
# Fix the station lacking species   

# check name of station(s)  
# xtabs(~addNA(STATION_CODE), subset(dat_3, is.na(Species)))

sel <- dat_3$STATION_CODE  == "02B"
dat_3$Species[sel] <- "Torsk"

# Check again
cat("\nCheck 3 \n")
```

```
## 
## Check 3
```

```r
xtabs(~addNA(Species), dat_3)
```

```
## addNA(Species)
## Skrubbe   Torsk    <NA> 
##      15     322       0
```

### Renaming  

```r
dat_4 <- dat_3 %>% 
  select(-Station) %>%
  rename(Fish_no = Fisknr,
         Length = Lengde, 
         Weight = Vekt,
         Liver_weight = Levervekt,
         Liver_color = Leverfarge,
         Gonad_weight = Gonadevekt,
         Age = Alder) %>%
  select(STATION_CODE, everything())
```

## Check lacking data   

### Summary

```r
apply(!is.na(dat_4), 2, mean)
```

```
## STATION_CODE      Species      Fish_no       Length       Weight        Kjønn Liver_weight  Liver_color Gonad_weight          Age 
##    1.0000000    1.0000000    1.0000000    1.0000000    1.0000000    0.9762611    0.9762611    0.9139466    0.3620178    0.7151335
```

### Fraction OK data per station

```r
dat_4 %>%
  select(-Species, -Kjønn, -Liver_color) %>%
  tidyr::pivot_longer(-STATION_CODE, names_to = "Variable", values_to = "Value") %>%
  group_by(STATION_CODE, Variable) %>%
  summarise(Fraction_ok = mean(is.finite(Value))) %>%
  tidyr::pivot_wider(names_from = "Variable", values_from = "Fraction_ok")
```

```
## `summarise()` has grouped output by 'STATION_CODE'. You can override using the `.groups` argument.
```

```
## # A tibble: 19 × 7
## # Groups:   STATION_CODE [19]
##    STATION_CODE   Age Fish_no Gonad_weight Length Liver_weight Weight
##    <chr>        <dbl>   <dbl>        <dbl>  <dbl>        <dbl>  <dbl>
##  1 02B          1           1       0           1        1          1
##  2 10B          0.882       1       0           1        0.882      1
##  3 13B          1           1       1           1        1          1
##  4 15B          0.789       1       0.789       1        1          1
##  5 19B          1           1       1           1        1          1
##  6 20B          1           1       1           1        1          1
##  7 23B          1           1       1           1        1          1
##  8 24B          1           1       0           1        1          1
##  9 28B          1           1       0           1        1          1
## 10 30B          0.293       1       0.259       1        0.897      1
## 11 33F          0           1       0           1        1          1
## 12 36B          1           1       0           1        1          1
## 13 43B          1           1       1           1        1          1
## 14 45B2         0.941       1       0           1        1          1
## 15 53B          0           1       0.75        1        1          1
## 16 71B          0.567       1       0           1        1          1
## 17 80B          1           1       0           1        1          1
## 18 96B          1           1       0           1        1          1
## 19 98B          1           1       0.0667      1        1          1
```

### Number of OK data per station

```r
dat_4 %>%
  select(-Species, -Kjønn, -Liver_color) %>%
  tidyr::pivot_longer(-STATION_CODE, names_to = "Variable", values_to = "Value") %>%
  group_by(STATION_CODE, Variable) %>%
  summarise(Number_ok = sum(is.finite(Value))) %>%
  tidyr::pivot_wider(names_from = "Variable", values_from = "Number_ok")
```

```
## `summarise()` has grouped output by 'STATION_CODE'. You can override using the `.groups` argument.
```

```
## # A tibble: 19 × 7
## # Groups:   STATION_CODE [19]
##    STATION_CODE   Age Fish_no Gonad_weight Length Liver_weight Weight
##    <chr>        <int>   <int>        <int>  <int>        <int>  <int>
##  1 02B              7       7            0      7            7      7
##  2 10B             15      17            0     17           15     17
##  3 13B             15      15           15     15           15     15
##  4 15B             15      19           15     19           19     19
##  5 19B             15      15           15     15           15     15
##  6 20B             15      15           15     15           15     15
##  7 23B             16      16           16     16           16     16
##  8 24B             15      15            0     15           15     15
##  9 28B             15      15            0     15           15     15
## 10 30B             17      58           15     58           52     58
## 11 33F              0      15            0     15           15     15
## 12 36B             15      15            0     15           15     15
## 13 43B             15      15           15     15           15     15
## 14 45B2            16      17            0     17           17     17
## 15 53B              0      20           15     20           20     20
## 16 71B             17      30            0     30           30     30
## 17 80B              3       3            0      3            3      3
## 18 96B             15      15            0     15           15     15
## 19 98B             15      15            1     15           15     15
```


## Test plot 1, length  


```r
ggplot(dat_4, aes(Length, Weight, color = Species)) +
  geom_point() +
  facet_wrap(~STATION_CODE)
```

![plot of chunk unnamed-chunk-25](figure/unnamed-chunk-25-1.png)

## Test plot 2, weight  


```r
ggplot(dat_4, aes(Weight, Gonad_weight, color = Species)) +
  geom_point() +
  facet_wrap(~STATION_CODE)
```

```
## Warning: Removed 215 rows containing missing values (geom_point).
```

![plot of chunk unnamed-chunk-26](figure/unnamed-chunk-26-1.png)

## Test plot 3, liver weight vs weight  


```r
ggplot(dat_4, aes(Weight, Liver_weight, color = Species)) +
  geom_point() +
  facet_wrap(~STATION_CODE)
```

```
## Warning: Removed 8 rows containing missing values (geom_point).
```

![plot of chunk unnamed-chunk-27](figure/unnamed-chunk-27-1.png)


## Test plot 4, age vs length  


```r
ggplot(dat_4, aes(Length, Age, color = Species)) +
  geom_point() +
  facet_wrap(~STATION_CODE)
```

```
## Warning: Removed 96 rows containing missing values (geom_point).
```

![plot of chunk unnamed-chunk-28](figure/unnamed-chunk-28-1.png)

## Save   
- Folder `Data`  

```r
# Wrapped in FALSE to avoid accidental overwriting

if (FALSE){
  
  saveRDS(
    dat_4, 
    paste0("Data/150_Fish_individual_data_", selected_year, ".rds")
  )
  
  write.csv(
    dat_4, 
    paste0("Data/150_Fish_individual_data_", selected_year, ".csv"),
    fileEncoding = "UTF-8") 
  
}
```


```r
# The "ordinary knit button" doesn't work because pandoc fails to convert the .md file to HTML
# Instead run the following line (must not be uncommented, that may create a loop):
#   knitr::knit("03_Read_fish_individual_data.Rmd", encoding = "UTF-8") 
```

