  

# How to "generalize" columns named after last year, e.g. 'EQSclass_2018' - see no. 14

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Set version number and year ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

version_no <- "01"    # REMEMBER TO CHANGE THIS! (but file will not be overwritten if you forget)
last_year <- 2019


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Libraries + functions ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# install safejoin
library(devtools)
install_github(repo = "moodymudskipper/safejoin")

library(dplyr)        
library(tidyr)        # gather()
library(purrr)        # map_ functions
library(safejoin)     # safe_left_join() - from https://github.com/moodymudskipper/safejoin   
library(stringr)
library(lubridate)
library(openxlsx)
library(mgcv)
library(AICcmodavg)   # AICc()

source("201_Make_big_excel_file_functions.R")
source("201_Time_series_write_to_Excel_functions.R", encoding = "UTF-8")

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Main data ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Medians
# Read and reformat the most recent data (by default)  
files <- dir("Data", pattern = "110_mediandata_updated") %>% rev()

cat("Reading the last file downloaded:")
cat("\n", files[1])
cat("\n")
cat("If you want to read a different file, replace 'files[1]' with the file you want")
cat("\n")

filename <- paste0("Data/", files[1]) 
data_med2 <- readRDS(filename) %>%
  rename(Proref_median = Median,
         Median = Value) 

# We save the date part of the text (e.g., '2020-04-23')
# This will be used in part 10, when we save the resulting file
file_date <- substr(filename, 29, 38)    # pick out the part of the text from character no. 17 to no. 26

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Other data ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Not needed?
# data_med2 <- readRDS(file = "Data/11_data_med_updated_02.rds")

# Trends
result_10yr <- readRDS("Data/120_result_10yr.RData")
result_long <- readRDS("Data/120_result_long.RData")

# Trends for previous year (2018)
result_10yr_prev <- readRDS("Input_data/120_result_10yr_2018.RData")
result_long_prev <- readRDS("Input_data/120_result_long_2018.RData")

# Individual data (for SD)
data_lastyear_ind <- readRDS(paste0("Data/101_data_updated_", file_date, ".rds"))    # %>%

#
# Check uniqueness
#
df <- data_lastyear_ind %>%
  group_by(STATION_CODE, LATIN_NAME, TISSUE_NAME, MYEAR, SAMPLE_NO2, UNIT, PARAM) %>%
  mutate(n = n()) %>%
  filter(n > 1)
nrow(df)  # should result in zero


# Last year's table
# results_last_year <- read.csv2("Input_data/Data_xl_lessthans_ver12.csv", encoding = "UTF-8")
results_last_year <- readRDS("Input_data/Data_xl_lessthans_ver12.rds")


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Fixing ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

### 1. Delete VDSI that are not Basis WW    
sel <- with(data_med2, PARAM %in% c("VDSI","VDSI/Intersex") & !Basis %in% "WW"); sum(sel)
data_med2 <- data_med2[!sel,]

# Check
# data_med2 %>% filter(PARAM %in% "VDSI" & MYEAR %in% 2015:2017 & STATION_CODE %in% c("227G","227G2"))

### 2. BAP unit
sel <- with(data_med2, PARAM %in% "BAP"); sum(sel)
# data_med2[sel,] %>% count(UNIT)
data_med2$UNIT[sel] <- "ug/kg/ABS 380 nm"



### 3. Check HG values in eider duck 2017 (now all are ok)

# Kept as it is a nice way to show if the current year deviates a lot from the previous year

sel <- with(data_med2, LATIN_NAME %in% "Somateria mollissima" &
              Basis %in% c("WW", "FB") &
              PARAM %in% "HG")
data_med2[sel,] %>% 
  mutate(Median = round(Median, 4)) %>%
  select(TISSUE_NAME, Basis, MYEAR, Median) %>%
  arrange(TISSUE_NAME, Basis, MYEAR)


### 4. MCCP values in eider duck 2017  
# These are actually OK

sel <- with(data_med2, LATIN_NAME %in% "Somateria mollissima" &
              Basis %in% c("WW", "FB") &
              PARAM %in% "MCCP")
data_med2[sel,] %>% 
  mutate(Median = round(Median, 4)) %>%
  select(TISSUE_NAME, Basis, MYEAR, Median) %>%
  arrange(TISSUE_NAME, Basis, MYEAR)




#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Checking data 1 ----
# One problematic species, one problematic station, one problematic parameter
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Lacking Eider data 2018?
# Lacking VDSI 2018?

cat("==================================\n")
cat("Eider duck\n")
cat("----------------------------------\n")
check <- data_med2 %>% filter(LATIN_NAME %in% "Somateria mollissima" & MYEAR %in% 2018 & Basis %in% "WW") %>%
  group_by(TISSUE_NAME, PARAM) %>%
  summarise(N = n()) %>%
  group_by(TISSUE_NAME, N) %>%
  summarise(PARAM = paste(PARAM, collapse = ", ")) %>%
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
  summarise(N = n()) %>%
  group_by(LATIN_NAME, PARAM, N) %>%
  summarise(MYEAR = paste(MYEAR, collapse = ", ")) %>%
  group_by(LATIN_NAME, N, MYEAR) %>%
  summarise(PARAM = paste(PARAM, collapse = ", ")) %>%
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
  summarise(N = n()) %>%
  group_by(STATION_CODE, N) %>%
  summarise(MYEAR = paste(MYEAR, collapse = ", ")) %>%
  group_by(MYEAR, N) %>%
  summarise(STATION_CODE = paste(STATION_CODE, collapse = ", ")) %>%
  as.data.frame()
# check
for (i in 1:nrow(check)){
  cat(check[i,"STATION_CODE"], "\nNumber of medians since 2008: N =", check[i,"N"], ":\n")
  cat(check[i,"MYEAR"], "\n\n")
}


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 01a. Make 'data_xlvalues' ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# This data set is used only for making select_data
data_for_select_data <- data_med2 %>%
  select(STATION_CODE, LATIN_NAME, PARAM, Basis, UNIT, MYEAR, Median) %>%
  group_by(STATION_CODE, LATIN_NAME, PARAM, Basis, UNIT) %>%
  mutate(Present_last7year = max(MYEAR) >= 2012) %>%              # Make sure it's updated 
  ungroup()

data_for_select_data %>%
  count(MYEAR >= 1980, !is.na(Median), Present_last7year, !is.na(UNIT))

select_data <- with(data_for_select_data, MYEAR >= 1980 & !is.na(Median) & Present_last7year & !is.na(UNIT))
nrow(data_med2)  # 447600
sum(select_data) # 163519

data_xlvalues1 <- data_med2[select_data,]
data_xlvalues <- data_xlvalues1 %>%
  select(MYEAR:UNIT, Basis, Median) %>%
  spread(MYEAR, Median) %>%
  as.data.frame()
nrow(data_xlvalues)  
# 24661

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 01b. Make less-than columns (as TRUE/FALSE) ----
# added at the end of the script but used in part 14
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

data_lessthans <- data_med2[select_data,] %>%
  mutate(Lessthan = Over_LOQ/N < 0.5) %>%
  select(MYEAR:UNIT, Basis, Lessthan) %>%
  # select(-STATION_NAME) %>%     # Some station names varies, e.g. 02B (Kirkøy nord or 'Kirkøy (north)')
  spread(MYEAR, Lessthan) %>%
  as.data.frame()

# MUST be equal
nrow(data_xlvalues)   # 24661
nrow(data_lessthans)  # 24661 - YES

# Change column names 
# We now use the names Yr_ and EQS_ for medians and EQS columns, because they are easier to search for
# Note: before writing to Excel we will change these to V.. and Q..
cn <- colnames(data_lessthans)
isnum <- !is.na(as.numeric(cn))
colnames(data_lessthans)[isnum] <- paste0("Lt_", cn[isnum])

# colnames(data_lessthans)
# str(data_lessthans)

nrow(data_lessthans)  # 24661
nrow(data_xlvalues)   # 24661    MUST BE EQUAL! (still)

# Check (all must be equal, ie all numbers below should be zero!)
sum(data_xlvalues$PARAM != data_lessthans$PARAM)
sum(data_xlvalues$STATION_CODE != data_lessthans$STATION_CODE)
sum(data_xlvalues$LATIN_NAME != data_lessthans$LATIN_NAME)
sum(data_xlvalues$TISSUE_NAME != data_lessthans$TISSUE_NAME)
sum(data_xlvalues$Basis != data_lessthans$Basis)

# head(data_lessthans,1)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 02. Check that we don't have any duplicates ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

checkdata <- data_xlvalues %>%
  group_by(STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM, Basis) %>%
  summarise(N = n()) %>%
  filter(N > 1) 
cat("Number of data with duplicates:", nrow(checkdata), "\n")

# Only if duplicates - check the first duplicate
if (nrow(checkdata) > 0){
  i <- 1
  df1 <- checkdata[i,] %>% 
    mutate(Key = paste(STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM, Basis, sep = "_"))
  df2 <- data_xlvalues %>% 
    mutate(Key = paste(STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM, Basis, sep = "_"))
  sel <- df2$Key %in% df1$Key; sum(sel)
  data_xlvalues[sel,]
  
  # 02B    Kirkøy nord 
  # 02B Kirkøy (north)
  
  # Check statoins last year
  results_last_year %>%
    filter(STATION_CODE %in% "02B") %>%
    count(STATION_CODE, Station.Name)
}

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 03. Change column names ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# We now use the names Yr_ and EQS_ for medians and EQS columns, because they are easier to search for
# Note: before writing to Excel we will change these to V.. and Q..
cn <- colnames(data_xlvalues)
isnum <- !is.na(as.numeric(cn))
# colnames(data_xlvalues)[isnum] <- paste0("Yr_", substr(cn[isnum], 3, 4))
colnames(data_xlvalues)[isnum] <- paste0("Yr_", cn[isnum])
colnames(data_xlvalues)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 04. Add EQS limits ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

EQS_limits <- read.xlsx("Input_data/EQS_limits.xlsx", "EQS")[,1:8]
EQS_limits <- fact2char_df(EQS_limits)
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
data_EQS <- left_join(data_EQS, EQS_limits[,c("PARAM","Limit")], by = "PARAM")
table(!is.na(data_EQS$Limit))
# FALSE  TRUE 
# 21617  3044 

# Remove limit where Basis isn'n WW or WWa
sel <- !data_EQS$Basis %in% c("WW","WWa")
mean(sel)
data_EQS$Limit[sel] <- NA
table(!is.na(data_EQS$Limit))

# Add "n" in rows with EQS and in the columns where 'data_xlvalues' has values
# 15 sec
sel_rows <- which(!is.na(data_EQS$Limit))
length(sel_rows)
# Pick the columns with values only ('data_xlvalues_vals')
data_EQS_vals <- data_EQS[,colno_values]
data_xlvalues_vals <- data_xlvalues[,colno_values]
for (i in sel_rows){
  data_EQS_vals[i, !is.na(data_xlvalues_vals[i,])] <- "n"
}
# Put the columns with values only back into data_EQS
data_EQS[,colno_values] <- data_EQS_vals

# Check that the columns fit together
i <- with(data_xlvalues, PARAM %in% "HG" & STATION_CODE %in% "10A2" & Basis == "WW")
i <- with(data_xlvalues, PARAM %in% "HG" & STATION_CODE %in% "30B" & Basis == "WW")
a <- data_xlvalues[i, ] %>% as.matrix(ncol = 1)
b <- data_EQS[i, 1:ncol(a)] %>% as.matrix(ncol = 1)
rbind(a, b)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 05. Make SD_last ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Get column names for copy-paste
# data_all %>% colnames() %>% paste(collapse = ", ")

data_all_long <- data_lastyear_ind %>%
  select(STATION_CODE, LATIN_NAME, TISSUE_NAME, MYEAR, SAMPLE_NO2, UNIT, PARAM,
         VALUE_WW, VALUE_DW, VALUE_FB, VALUE_WWa, VALUE_DWa, VALUE_FBa) %>%
  tidyr::gather("Basis", "VALUE", VALUE_WW, VALUE_DW, VALUE_FB, VALUE_WWa, VALUE_DWa, VALUE_FBa) %>%
  mutate(Basis = stringr::str_sub(Basis, start = -2))
str(data_all_long)

yr <- 2019
df_sd_last <- data_all_long %>%
  filter(MYEAR %in% yr) %>%
  group_by(PARAM, STATION_CODE, LATIN_NAME, TISSUE_NAME, Basis) %>%
  summarise(SD_last = sd(VALUE, na.rm = TRUE))
  
# Check
df_sd_last %>%
  filter(PARAM %in% "BDE99" & STATION_CODE %in% "76A2" & Basis %in% "WW")

# Check (snails - sample size = 1, so no SD can be defined)
df_sd_last %>%
  filter(PARAM %in% "TBT" & STATION_CODE %in% "71G" & Basis %in% "WW")

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 06. Start building data_xl ----
# the data frame that eventually will be written to Excel
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 06a. List of station names ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

data_stations <- readxl::read_excel("Input_data/Kartbase_edit.xlsx")


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 06b. Make data_xl ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

data_xl <- data_xlvalues %>% 
  rename(Unit = UNIT) %>% select(STATION_CODE, TISSUE_NAME, LATIN_NAME, PARAM, Basis, Unit)
ncol(data_xl) # 6

#
# Get extra columns for parameters and stations
#
df_par <- read.csv("Input_data/Lookup for big excel - param.csv", sep = ";", dec = ",", stringsAsFactors = FALSE)
df_stat <- readxl::read_excel("Input_data/Lookup for big excel - stations.xlsx")
colnames(df_par)[1] <- "PARAM"
colnames(df_stat)[1] <- "STATION_CODE"

# 227G1 -> 227G
sel <- df_stat$STATION_CODE %in% "227G1"; sum(sel)
df_stat$STATION_CODE[sel] <- "227G"

# Fix encoding errors in VAnnforekomstNavn
df_stat$VAnnforekomstNavn <- fix_utf8(df_stat$VAnnforekomstNavn)
# head(df_par, 2)
# head(df_stat, 2)
# head(data_xl, 2)

# Add new names
df_stat$Station.Name <- NULL
df_stat <- left_join(
  df_stat, 
  data_stations %>% select("STATION_CODE","Station_name"), 
  by = "STATION_CODE")

# Correct column order
# dput(colnames(df_stat))
cols <- c("STATION_CODE", "Station_name", "Area", "County", "Water.Region", "VannforekomstID", "VAnnforekomstNavn")
df_stat <- df_stat[,cols]
colnames(df_stat)[2] <- "Station.Name"

#
# Add parameter columns
#
# Some IUPAC values contain semicolon, which makes a mess in Excel
# We replace the semicolon by a slash
# View(df_par)
sel <- grepl(";", df_par$IUPAC, fixed = TRUE)
sum(sel)
df_par$IUPAC[sel]
df_par$IUPAC <- gsub(";", " / ", df_par$IUPAC, fixed = TRUE)
df_par$IUPAC[sel]

# Change sum parameter names som they fit with the our "new" names
# Also see section 28
sel <- df_par$Parameter.Code %in% "PK_S"; sum(sel)
if (sum(sel) > 0)
  df_par$Parameter.Code[sel] <- "KPAH"
sel <- df_par$Parameter.Code %in% "PAHSS"; sum(sel)
if (sum(sel) > 0)
  df_par$Parameter.Code[sel] <- "PAH16"

# Add extra columns
before <- nrow(data_xl)  #  28296
data_xl <- left_join(data_xl, df_par, by = "PARAM")
after1 <- nrow(data_xl)
if (before != after1)
  message("*************** Number of rows changed! **************")
data_xl <- left_join(data_xl, df_stat, by = "STATION_CODE")
after2 <- nrow(data_xl)  #  28296
if (after1 != after2)
  message("*************** Number of rows changed! **************")

# Put columns in correct sequence
cols_sequence <- c("PARAM", "Parameter.Name", "IUPAC", "CAS", "Component.Name", "Substance.Group", "Unit",
                   "STATION_CODE", "Station.Name", "Area", "County", "Water.Region", "VannforekomstID", "VAnnforekomstNavn",
                   "LATIN_NAME", "TISSUE_NAME",  "Basis")
data_xl <- data_xl[,cols_sequence]

# If error, try:
# cols_sequence[!cols_sequence %in% colnames(data_xl)]

#
# Add Svalbard manually
#
sel <- data_xl$STATION_CODE %in% "19B"; sum(sel)
data_xl$Station.Name[sel] <- "Svalbard"
data_xl$County[sel] <- "Svalbard"

sel <- data_xl$STATION_CODE %in% "19N"; sum(sel)
data_xl$Station.Name[sel] <- "Breøyane"
data_xl$County[sel] <- "Svalbard"

sel <- data_xl$STATION_CODE %in% "I964"; sum(sel)
data_xl$Station.Name[sel] <- "Toraneskaien"
data_xl$County[sel] <- "Nordland"

sel <- data_xl$STATION_CODE %in% "227G2"; sum(sel)
data_xl$Station.Name[sel] <- "Flatskjær"
data_xl$County[sel] <- "Rogaland"

sel <- data_xl$STATION_CODE %in% "76A2"; sum(sel)
data_xl$Station.Name[sel] <- "Risøy"
data_xl$County[sel] <- "Aust-Agder"

sel <- data_xl$STATION_CODE %in% "97A3"; sum(sel)
data_xl$Station.Name[sel] <- "Bodø harbour"
data_xl$County[sel] <- "Nordland"

sel <- data_xl$STATION_CODE %in% "28A2"; sum(sel)
data_xl$Station.Name[sel] <- "Ålesund harbour"
data_xl$County[sel] <- "Møre og Romsdal"
data_xl$Water.Region[sel] <- "Møre og Romsdal"
data_xl$VAnnforekomstNavn[sel] <- "Borgundfjorden-vest"

sel <- data_xl$STATION_CODE %in% "I911"; sum(sel)
data_xl$Station.Name[sel] <- "Horvika"
data_xl$County[sel] <- "Møre og Romsdal"

sel <- data_xl$STATION_CODE %in% "I914"; sum(sel)
data_xl$Station.Name[sel] <- "Flåøya (southeast)"
data_xl$County[sel] <- "Møre og Romsdal"

sel <- data_xl$STATION_CODE %in% "I132"; sum(sel)
data_xl$Station.Name[sel] <- "Svensholmen"
data_xl$County[sel] <- "Vest-Agder"
data_xl$Water.Region[sel] <- "Agder"
data_xl$VAnnforekomstNavn[sel] <- "Kristiansandsfjorden-indre"

cat("\nNumber of columns:", ncol(data_xl), "\n") # 17

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 07. Add PROREF (background values) and add median values ---- 
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# NOTE two variants, the original, and a variant based on reading former background stations (recycling)
#   and calculate PROREF (ie C95) for everything not WW (DW, etc)
#
# NOTE: check which of the two variants are uncommented!
#

#
# Original, use this!
#
# load(file = "Data/13b5_res_list.RData")     # res_list
# df_background <- res_list %>% bind_rows()                           # keep all rows !
# nrow(df_background) # 1175


#
# If we have "recreated" 'df_background2' based on stations and Q95 (Proref) values that was delivered before (see end of 13b5 script), use THIS:
#
if (FALSE){
  load(file = "Input_data/13b5_df_background2.RData")     # df_background2
  nrow(df_background2) # 1214
  df_background <- df_background2
}

#
# LATEST VERSION - use same code as in script 11
#

if (TRUE){
  # Old ones 
  load("../Milkys_2018/Input_data/13b5_df_background2.RData")  # df_background2
  head(df_background2, 3)
  
  # New ones (without "Bxx" stations for blue mussel), 
  proref_ww <- readxl::read_excel("Data/51 Proref - ww for selected determinants.xlsx") %>%
    mutate(Basis = stringr::str_sub(Variable, start = 7)) %>%
    rename(Stations = Stations_proref,
           N_stations = N_stations_proref, 
           N = N_proref)
  
  # df_background2 %>% colnames()
  # proref_ww %>% colnames()
  
  # proref_updated01 = old data set, with selected columns,
  #   and removing the rows that were produced for the paper (proref_ww)
  proref_updated01 <- df_background2 %>%
    select(PARAM, LATIN_NAME, TISSUE_NAME, Basis, Stations, N_stations, N, Median, Q95) %>%
    anti_join(proref_ww %>% select(PARAM, LATIN_NAME, TISSUE_NAME, Basis),
              by = c("PARAM", "LATIN_NAME", "TISSUE_NAME", "Basis")
    )
  
  # proref_updated02 - adding the new rows from paper
  proref_updated02 <- proref_updated01 %>%
    bind_rows(
      proref_ww %>%
        select(PARAM, LATIN_NAME, TISSUE_NAME, Basis, Stations, N_stations, N, Median, Q95)
    )
  
  nrow(df_background2)    # 1214
  nrow(proref_updated01)  # 1168
  nrow(proref_updated02)  # 1215
  
  df_background <- proref_updated02
  
  # Used in report - copy to Norman's K (K:\Avdeling\Mar\NOG\JMG\2018\Tabeller)
  openxlsx::write.xlsx(proref_updated02, "Data/13_proref_updated02.xlsx")
  
}


#
# Adding "Stations", "N_stations", "N", "Median", "Q95"
#
# head(df_background , 3)
# dput(colnames(df_background))
cols <- c("PARAM", "LATIN_NAME", "TISSUE_NAME", "Basis",
          "Stations", "N_stations", "N", "Median", "Q95")
data_xl <- left_join(data_xl, df_background[,cols], by = c("PARAM", "LATIN_NAME", "TISSUE_NAME", "Basis"))
nrow(data_xl)  #  15337
ncol(data_xl)  #  22

# Change some column names
colnames(data_xl)[colnames(data_xl) %in% "Stations"] <- "Backgr_stations"
colnames(data_xl)[colnames(data_xl) %in% "N_stations"] <- "Backgr_Nstat"
colnames(data_xl)[colnames(data_xl) %in% "N"] <- "Backgr_N"

# head(data_xl, 2)

# Prepare for adding values from 'data_xlvalues' and EQS sign from 'data_EQS'
ind_cols1 <- which(grepl("Yr_", colnames(data_xlvalues)))
ind_cols2 <- which(grepl("EQS_", colnames(data_EQS)))
length(ind_cols1) == length(ind_cols2)  # should be TRUE

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

dim(data_xl)
# 25383   100 (correct - was 96 in 2017 with data only through 2016, and 98 in 2018)

# colnames(data_xl)
# str(data_xl)

cat("\nNumber of columns:", ncol(data_xl), "\n") # 100


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 08. Add SD, PROREF etc. for second last year (2017) ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# 5 columns: SD 2015	PROREF 2015	EAC 2015	EQS 2015	Sample count 2016

cols <- c("PARAM", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE", "Basis",
          paste0("Ant.prøver.", last_year - 1),
          "SD_last", 
          paste0("Klasse.", last_year - 1), 
          paste0("EQSclass_", last_year - 1), 
          "EQS")

# cols %in% colnames(results_last_year)
data_xl <- left_join(data_xl, results_last_year[,cols], 
                     by = c("PARAM", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE", "Basis"))
dim(data_xl) # 25383   105

# Change column name 1
colnumber <- which(colnames(data_xl) == "EQS"); colnumber
colnames(data_xl)[colnumber] <- paste0("EQSthreshold_", last_year - 1)

# Change column name 2
colnumber <- which(colnames(data_xl) == "SD_last"); colnumber
colnames(data_xl)[colnumber] <- paste0("SD.", last_year - 1)


cat("\nNumber of columns:", ncol(data_xl), "\n") # 105


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 09. Add N_string (sample size last year) ----
#
# Example: N_string "8 (6-3)" means that: 
# - number of samples (pooled or unpooled) = 8
# - number of pooled samples = 8
# - maximum number of individuals per pooled sample = 3
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# For getting the number of individuals (per pooeld sample) from X_BULK_BIO 
# Function based on counting the number of commas
number_of_ind <- function(txt){
  result <- regexec(",", txt, fixed = TRUE) %>% map_int(~.[[1]]) + 1
  result[is.na(result)] <- 1
  result[result == 0] <- 1
  result
}

# Get number of individuals per pooled sample
# Use df_samples (table from LABWARE) 
df_samples_labware <- readRDS(file = "Data/01_df_samples.rds") %>%
  rename(STATION_CODE = AQUAMONITOR_CODE,
         LATIN_NAME = SPECIES,
         SAMPLE_NO2 = BIOTA_SAMPLENO) %>%
  mutate(TISSUE_NAME = stringr::str_sub(TISSUE, start = 4),
         No_individuals = number_of_ind(X_BULK_BIO)) %>%    # get number of individuals per pooled sample
  select(STATION_CODE, LATIN_NAME, TISSUE_NAME, SAMPLE_NO2, X_BULK_BIO, No_individuals) %>%
  group_by(STATION_CODE, LATIN_NAME, TISSUE_NAME, SAMPLE_NO2) %>%
  summarise_all(first) %>%
  ungroup() %>%
  # Special case for molluscs, which have no X_BULK_BIO value:
  mutate(No_individuals = case_when(
    LATIN_NAME %in% "Mytilus edulis" ~ 50,
    LATIN_NAME %in% c("Littorina littorea", "Nucella lapillus") ~ 30,
    No_individuals == 0 ~ 1,     # in the case of fish, no X_BULK_BIO value means 1 individual
    TRUE ~ No_individuals)
  )

# Check that values are unique (result should be 0)
# Not needed as we use safe_left_join with check V
df_samples_labware %>%
  count(STATION_CODE, LATIN_NAME, TISSUE_NAME, SAMPLE_NO2) %>%
  filter(n > 1) %>%
  nrow()

# Raw data with 'No_individuals' (info on pooled sample) added 
df_samples <- data_lastyear_ind %>%
  filter(MYEAR %in% 2018) %>%
  safe_left_join(subset(df_samples_labware, select = -X_BULK_BIO),
                 by = c("STATION_CODE", "LATIN_NAME", "TISSUE_NAME", "SAMPLE_NO2"),
                 check = "BCV")

# df_numbers - contains 'N_string' which will be added to data by left-join 
#  - equals raw data summarised per station x parameter
df_numbers <- df_samples %>%
  filter(!is.na(VALUE_WW)) %>%
  group_by(STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM) %>%
  summarise(N = n(),
            N_pooled = sum(No_individuals > 1),
            Max_ind = max(No_individuals)
            ) %>%
  mutate(
    N_string = paste0(N, " (", N_pooled, "-", Max_ind, ")")  # Make string
  )

#
# Imposex and intersex   
#
data_imposex <- readRDS("Data/32_data_imposex.rds")

df_numbers_imposex <- data_imposex %>%
  filter(Sex %in% "f" & PARAM %in% c("VDSI","Intersex") & !is.na(VALUE_WW)) %>%
  group_by(STATION_CODE) %>%
  summarise(Max_ind = n()) %>%
  mutate(
    N_string_new = paste0(1, " (", 1, "-", Max_ind, ")")
  ) %>%
  select(STATION_CODE, N_string_new)

#
# Replace N_string values for imposex and intersex 
#
df_numbers <- df_numbers %>%
  safe_left_join(df_numbers_imposex, by = c("STATION_CODE")) %>%
  mutate(N_string = ifelse(is.na(N_string_new), N_string, N_string_new)) %>%
  select(-N_string_new)

nrow(data_xl)  # 19214
data_xl <- safe_left_join(data_xl, 
                          df_numbers[,c("LATIN_NAME", "TISSUE_NAME", "STATION_CODE", "PARAM", "N_string")],
                          by = c("LATIN_NAME", "TISSUE_NAME", "STATION_CODE", "PARAM"),
                          check = "BCV")
nrow(data_xl)  # 19214

# 
# This sample size is only valid for the last year, so we delete it if there is no data in the last year
#
colnumber <- which(colnames(data_xl) == paste0("Yr_", last_year))
sel <- is.na(data_xl[[colnumber]]); sum(sel)
data_xl$N_string[sel] %>% is.na() %>% mean() # already NA for 98.6%
data_xl$N_string[sel] <- NA

# 
# For samples without pooled samples (e.g. "15 (0-1)"), replace with just the number ("15")
#
sel <- grepl ("(0-1)", data_xl$N_string, fixed = TRUE); sum(sel)  # just for checking
data_xl$N_string <- sub("(0-1)", "", data_xl$N_string, fixed = TRUE)

cat("\nNumber of columns:", ncol(data_xl), "\n") # 106



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 10. Add SD (for the last year) ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

data_xl_b <- data_xl  # backup
nrow(data_xl)  # 28296
data_xl <- left_join(data_xl, df_sd_last, by = c("PARAM", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE", "Basis"))
nrow(data_xl)  # 28296

cat("\nNumber of columns:", ncol(data_xl), "\n") # 107


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 11. Add Class for last year ----
#
# Class for second last year added below (section 27b)
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

colnumber_value <- which(colnames(data_xl) == paste0("Yr_", last_year)); colnumber_value
value_lastyear <- data_xl[,colnumber_value]

colnumber_lessthan <- which(colnames(data_lessthans) == paste0("Lt_", last_year)); colnumber_lessthan
lessthan_lastyear <- data_xl[,colnumber_value]

# Less-thans are set a tad lower, to be put in the lower class
sel <- !is.na(lessthan_lastyear) & lessthan_lastyear
value_lastyear[sel] <- value_lastyear[sel] - 0.00001

# Just to check that we get the correct classes, i.e., if the conc. is on the limit, we get the upper class (using right = FALSE)
check_classes <- cut(value_lastyear/data_lessthans$Q95, breaks = c(-999999,1,2,5,10,20,999999), right = FALSE)
levels(check_classes)

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


nrow(data_xl)  # 19304

cat("\nNumber of columns:", ncol(data_xl), "\n") # 108

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 12. EQS class - put variable without data ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#
# REPLACE YEAR
#

# Where EAC used to be, we put EQS class

# before:
# data_xl$EAC_2016 <- NA

# Now just give it a NA, then we set it after we have inserted the column for the EQS limit
# Then we will change the name, also 
data_xl$EQSclass_lastyear <- NA

cat("\nNumber of columns:", ncol(data_xl), "\n") # 109


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 13. Add EQS limit (WW and WWa only) ----   
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

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
head(df)

data_xl_b <- data_xl  # backup
data_xl <- safe_left_join(data_xl, df, by = c("PARAM", "Basis"), check = "V")

# colnames(data_xl)
cat("\nNumber of columns:", ncol(data_xl), "\n") # 110


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 14. Set values of EQS class ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#
# YEAR made general (uses only variable 'last_year')
#

value_lastyear <- data_xl[[paste0("Yr_", last_year)]]
lessthan_lastyear <- data_lessthans[[paste0("Lt_", last_year)]]

# Less-thans are set a tad lower, to be put in the lower class
sel <- !is.na(lessthan_lastyear) & lessthan_lastyear; sum(sel)
value_lastyear[sel] <- value_lastyear[sel] - 0.00001

# Fill variable with values
data_xl$EQSclass_lastyear <- cut(value_lastyear/data_xl$EQS, breaks = c(-999999,1,999999), right = FALSE, labels = FALSE)

# Change column name of last added variable
colnumber <- which(colnames(data_xl) %in% "EQSclass_lastyear")
colnames(data_xl)[colnumber] <- paste0("EQSclass_", last_year)

cat("\nNumber of columns:", ncol(data_xl), "\n") # still 110


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 15. Add OC ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


data_xl$OC <- NA

cat("\nNumber of columns:", ncol(data_xl), "\n") # 111


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 16. Add trends for last year  ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# 11 columns

source("13_Time_series_write_to_Excel_functions.R")

cols <- c("PARAM", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE", "Basis")
trend_long_for_excel <- make_trend_data_for_excel2(result_long, data_xl[,cols])
trend_10yr_for_excel <- make_trend_data_for_excel2(result_10yr, data_xl[,cols])

# head(trend_long_for_excel, 2)

# Combine log + 10yr and prepare even more for Excel
trends_for_excel <- combine_long_and_short_trends_for_excel2(trend_long_for_excel, trend_10yr_for_excel)

# head(trends_for_excel, 2)
nrow(trends_for_excel)  # 29582

# Change column name
colnumber <- which(colnames(trends_for_excel) %in% "Trend.year")
colnames(trends_for_excel)[colnumber] <- paste0("Trends.", last_year)
colnames(trends_for_excel)

check <- data_xl %>%
  group_by(STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM, Basis) %>%
  summarise(N = n())
cat("Number of duplicates:", sum(check$N > 1), "\n")   

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
cat("Number of duplicates:", sum(check$N > 1), "\n")   

if ( sum(check$N > 1)){
  table(check$N)
  check %>% filter(N > 1) %>% head(10)
  df1 <- check %>% filter(N > 1) %>% head(1)
  check2 <- trends_for_excel %>% 
    filter(STATION_CODE %in% df1$STATION_CODE, LATIN_NAME %in% df1$LATIN_NAME, 
           TISSUE_NAME %in% df1$TISSUE_NAME, PARAM %in% df1$PARAM, Basis %in% df1$Basis)
  check2
}

#
# Add columns to data
#

data_xl_b <- data_xl  # backup
data_xl <- safe_left_join(data_xl, trends_for_excel, by = cols, check = "V")

cat("\nNumber of columns:", ncol(data_xl), "\n") # 122
# colnames(data_xl) 

# data_xl <- data_xl_b  # restore from backup, if needed


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 17. Add trends from second last year ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# 11 columns


#o#o#o#o#o#o#o#o#o#o#o#o#o#o
### a. Get trends and change col. names
#o#o#o#o#o#o#o#o#o#o#o#o#o#o

cols <- c("PARAM", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE", "Basis")
trend_long_for_excel <- make_trend_data_for_excel2(result_long_prev, data_xl[,cols]) # Only change: Adding _prev here
trend_10yr_for_excel <- make_trend_data_for_excel2(result_10yr_prev, data_xl[,cols]) # Only change: Adding _prev here

# head(trend_long_for_excel, 2)

# Combine log + 10yr and prepare even more for Excel
trends_for_excel <- combine_long_and_short_trends_for_excel2(trend_long_for_excel, trend_10yr_for_excel)


# head(trends_for_excel, 2)
nrow(trends_for_excel)  # 29546


#
# Change names 10 columns
#
colnames(trends_for_excel)

var2 <- c("Trend p(long)", "Detectable % change(long)", "First Year(long)", "Last Year(long)",
          "No of Years(long)", "Trend p(short)", "Detectable % change(short)", "First Year(short)", "Last Year(short)",
          "No of Years(short)")
if (sum(colnames(trends_for_excel) %in% var2) != 10){
  message("Warning: one of the names doesn't fit (should be 10 names fitting)")
}
var2_new <- c(paste(var2, last_year - 1))

# Set new names
for (i in 1:length(var2_new)){
  sel <- colnames(trends_for_excel) == var2[i]
  colnames(trends_for_excel)[sel] <- var2_new[i]
}

# Change column 11
colnumber <- which(colnames(trends_for_excel) %in% "Trend.year")
colnames(trends_for_excel)[colnumber] <- paste0("Trends.", last_year - 1)
colnames(trends_for_excel)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o
### b. Check duplicates
#o#o#o#o#o#o#o#o#o#o#o#o#o#o

check <- data_xl %>%
  group_by(STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM, Basis) %>%
  summarise(N = n())
cat("Number of duplicates:", sum(check$N > 1), "\n")   

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
cat("Number of duplicates:", sum(check$N > 1), "\n")   

if ( sum(check$N > 1)){
  table(check$N)
  check %>% filter(N > 1) %>% head(10)
  df1 <- check %>% filter(N > 1) %>% head(1)
  check2 <- trends_for_excel %>% 
    filter(STATION_CODE %in% df1$STATION_CODE, LATIN_NAME %in% df1$LATIN_NAME, 
           TISSUE_NAME %in% df1$TISSUE_NAME, PARAM %in% df1$PARAM, Basis %in% df1$Basis)
  check2
}

#o#o#o#o#o#o#o#o#o#o#o#o#o#o
### c. add trends to data_xl
#o#o#o#o#o#o#o#o#o#o#o#o#o#o

data_xl_b <- data_xl    # backup
# data_xl <- data_xl_b  # revert to backup

data_xl <- safe_left_join(
  data_xl, 
  trends_for_excel, 
  by = cols,
  check = "V")

cat("\nNumber of columns:", ncol(data_xl), "\n") # 133


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 18. Add "Last_two_years" +  "DETLIM_..." for second last year ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#
# Last_two_years
#
data_xl$Last_two_years <- NA

#
# DETLIM for seccond last year
#
cols <- c("PARAM", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE", "Basis", paste0("Det_limit_", last_year-1))

# dput(colnames(results_last_year))
# results_last_year[1, cols[1:5]]

data_xl_b <- data_xl  # backup
# data_xl <- data_xl_b

data_xl <- safe_left_join(
  data_xl, 
  results_last_year[,cols], 
  by = c("PARAM", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE", "Basis"),
  check = "V")

cat("\nNumber of columns:", ncol(data_xl), "\n") # 135


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 19. Add "DETLIM_..." for last year ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

cols <- c("PARAM", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE", "Basis", "Det_limit")

# dput(colnames(results_last_year))
# results_last_year[1, cols[1:5]]

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
  check = "V")

cat("\nNumber of columns:", ncol(data_xl), "\n") # 136


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 20. Add trend and EQS change ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# 4 columns: TREND_CHANGE	CLASS_CHANGE	EQS_CHANGE	EAC_CHANGE

# TREND_CHANGE
col_last <- paste0("Trends.", last_year)
col_seclast <- paste0("Trends.", last_year - 1)
sel <- !is.na(data_xl[,col_seclast]) & !is.na(data_xl[,col_last]) & data_xl[,col_seclast] != data_xl[,col_last]
sum(sel)
data_xl$TREND_CHANGE <- NA
data_xl$TREND_CHANGE[sel] <- paste(data_xl[sel,col_seclast], "to", data_xl[sel,col_last])

# CLASS_CHANGE
col_last <- paste0("Klasse.", last_year)
col_seclast <- paste0("Klasse.", last_year - 1)
sel <- !is.na(data_xl[,col_seclast]) & !is.na(data_xl[,col_last]) & data_xl[,col_seclast] != data_xl[,col_last]
sum(sel)
data_xl$CLASS_CHANGE <- NA
data_xl$CLASS_CHANGE[sel] <- paste(data_xl[sel,col_seclast], "to", data_xl[sel,col_last])

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
data_xl$EQS_CHANGE[sel] <- paste(EQSclass_seclast[sel], "to", EQSclass_last[sel])
table(data_xl$EQS_CHANGE)

# 1 to 2 2 to 1 
#      4     11

# EAC_CHANGE is not used
data_xl$EAC_CHANGE <- NA

cat("\nNumber of columns:", ncol(data_xl), "\n") # 140


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 21. Add D.d.i. ----
# D.d.i. = detectable data information = "N>LOQ(min-maks)"  
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# One column  
# Example:
#   7 [0.11 - 0.29] means 7 measurements over LOQ, and these measurements varied from 0.11 to 0.29  
# WE already have number of measurements over LOQ (from data_med)

df_ddi <- data_med2[data_med2$MYEAR %in% last_year,
                    c("PARAM", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE", "Basis", "N", "Over_LOQ")]


#
# Check what kind of data set we have and change 'df' accordingly
#
all_basis_present <- 
  c("VALUE_WW", "VALUE_DW", "VALUE_FB", "VALUE_WWa", "VALUE_DWa", "VALUE_FBa") %in% colnames(data_lastyear_ind)

if (mean(all_basis_present)==1){
  df <- data_lastyear_ind
} else {
  df <- data_lastyear_ind %>%
    rename(VALUE_WW = VALUE) %>%
    mutate(VALUE_DW = NA,
           VALUE_FB = NA,
           VALUE_WWa = NA,
           VALUE_DWa = NA,
           VALUE_FBa = NA
           )
}

# OLD
df_minmax <- data_lastyear_ind %>%
  select(STATION_CODE, LATIN_NAME, TISSUE_NAME, MYEAR, SAMPLE_NO2, UNIT, PARAM,
         VALUE_WW, VALUE_DW, VALUE_FB, VALUE_WWa, VALUE_DWa, VALUE_FBa) %>%
  tidyr::gather("Basis", "Value", VALUE_WW, VALUE_DW, VALUE_FB, VALUE_WWa, VALUE_DWa, VALUE_FBa) %>%
  mutate(Basis = sub("VALUE_", "", Basis)) %>%
  group_by(STATION_CODE,LATIN_NAME,TISSUE_NAME,PARAM,Basis) %>%
  summarise(ddi_min = round(min(Value, na.rm = TRUE),4), 
            ddi_max = round(max(Value, na.rm = TRUE),4)
            )

# NEW
# Get values
df_minmax_value <- data_lastyear_ind %>%
  filter(MYEAR %in% 2018) %>%
  select(STATION_CODE, LATIN_NAME, TISSUE_NAME, MYEAR, SAMPLE_NO2, UNIT, PARAM,
         VALUE_WW, VALUE_DW, VALUE_FB, VALUE_WWa, VALUE_DWa, VALUE_FBa) %>%
  tidyr::gather("Basis", "Value", VALUE_WW, VALUE_DW, VALUE_FB, VALUE_WWa, VALUE_DWa, VALUE_FBa) %>%
  mutate(Basis = sub("VALUE_", "", Basis))

# Get LOQ flags
df_minmax_flag <- data_lastyear_ind %>%
  filter(MYEAR %in% 2018) %>%
  select(STATION_CODE, LATIN_NAME, TISSUE_NAME, MYEAR, SAMPLE_NO2, UNIT, PARAM,
         Flag_WW)

# Add LOQ flags
df_minmax <- df_minmax_value %>%
  safe_left_join(df_minmax_flag,
                 by = c("STATION_CODE", "LATIN_NAME", "TISSUE_NAME", "MYEAR", 
                        "SAMPLE_NO2", "UNIT", "PARAM"),
                 check = "V")

df_ddi <- df_minmax %>%
  group_by(STATION_CODE,LATIN_NAME,TISSUE_NAME,PARAM,Basis) %>%
  summarise(ddi_min = round(min(Value[is.na(Flag_WW)], na.rm = TRUE),4), 
            ddi_max = round(max(Value[is.na(Flag_WW)], na.rm = TRUE),4),
            Over_LOQ = sum(is.na(Flag_WW))
  )


####
df_minmax %>%
  filter(PARAM == "4-N-NP" & STATION_CODE == "02B" & Basis == "WW")

df_ddi %>%
  filter(PARAM == "4-N-NP" & STATION_CODE == "02B")

df_ddi$DDI <- as.character(NA)
sel <- with(df_ddi, Over_LOQ > 1 & is.finite(ddi_min) & !is.na(ddi_min))
df_ddi$DDI[sel] <- with(df_ddi[sel,], paste0(Over_LOQ, " (", ddi_min, "-", ddi_max, ")")) 
sel <- with(df_ddi, Over_LOQ == 1 & is.finite(ddi_min) & !is.na(ddi_min))
df_ddi$DDI[sel] <- with(df_ddi[sel,], paste0(Over_LOQ, " (", ddi_min, ")")) 

df_ddi %>% head()
# df_ddi %>% View()

cols <- c("PARAM", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE", "Basis", "DDI")

data_xl_b <- data_xl  # backup

data_xl <- safe_left_join(
  data_xl, 
  df_ddi[,cols], 
  by = c("PARAM", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE", "Basis"),
  check = "V")

# head(subset(data_xl, PARAM == "CB28" & STATION_CODE == "02B") %>% as.data.frame())

cat("\nNumber of columns:", ncol(data_xl), "\n") # 141


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 22. Change tissue names to English ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#
# data_xl
#
xtabs(~addNA(TISSUE_NAME), data_xl)
data_xl <- data_xl %>%
  mutate(TISSUE_NAME_new = 
           case_when(TISSUE_NAME %in% "Blod" ~ "Blood",
                     TISSUE_NAME %in% "Egg homogenate of yolk and albumin" ~ "Egg",
                     TISSUE_NAME %in% "Galle" ~ "Bile",
                     TISSUE_NAME %in% "Muskel" ~ "Muscle",
                     TISSUE_NAME %in% "Lever" ~ "Liver",
                     TISSUE_NAME %in% "WO" ~ "Whole organism",
                     TRUE ~ TISSUE_NAME)
  )
xtabs(~addNA(TISSUE_NAME_new), data_xl)

# Replace the original TISSUE_NAME
data_xl$TISSUE_NAME <- data_xl$TISSUE_NAME_new
data_xl$TISSUE_NAME_new <- NULL

#
# data_lessthans
#
xtabs(~addNA(TISSUE_NAME), data_lessthans)
data_lessthans <- data_lessthans %>%
  mutate(TISSUE_NAME_new = 
           case_when(TISSUE_NAME %in% "Blod" ~ "Blood",
                     TISSUE_NAME %in% "Egg homogenate of yolk and albumin" ~ "Egg",
                     TISSUE_NAME %in% "Galle" ~ "Bile",
                     TISSUE_NAME %in% "Muskel" ~ "Muscle",
                     TISSUE_NAME %in% "Lever" ~ "Liver",
                     TISSUE_NAME %in% "WO" ~ "Whole organism",
                     TRUE ~ TISSUE_NAME)
  )
xtabs(~addNA(TISSUE_NAME_new), data_lessthans)

# Replace the original TISSUE_NAME
data_lessthans$TISSUE_NAME <- data_lessthans$TISSUE_NAME_new
data_lessthans$TISSUE_NAME_new <- NULL


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 23. Some checks ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# colnames(data_xl)
data_xl %>% filter(PARAM %in% "EROD") %>% select(STATION_CODE, N_string) %>% head(3)
data_xl %>% filter(PARAM %in% "VDSI") %>% select(STATION_CODE, N_string) %>% head(3)
data_xl %>% filter(STATION_CODE %in% "36G") %>% select(PARAM, N_string) %>% head(3)

data_xl %>% filter(PARAM %in% "VDSI" & !is.na(Yr_2017)) %>% select(STATION_CODE, Yr_2015:Yr_2018, N_string)
data_xl %>% filter(PARAM %in% "VDSI" & !is.na(Yr_2017)) %>% select(STATION_CODE, Yr_2013:Yr_2018)



# Comparing column names
if (FALSE){
  c1_prev <- which(colnames(results_last_year) %in% "EQS_2017") + 1
  c2_prev <- which(colnames(results_last_year) %in% "DDI")
  c1_curr <- which(colnames(data_xl) %in% "EQS_2018") + 1
  c2_curr <- which(colnames(data_xl) %in% "DDI")
  cbind(colnames(results_last_year)[c1_prev:c2_prev], 
        colnames(data_xl)[c1_curr:c2_curr])
}


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 25. Add trends as given 2016 ----
#
# *Not* second last year - fixed to 2016
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# Using the old OSPAR rules for less-thans
var1 <- c("PARAM", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE", "Basis")

# Just for checking....
if (FALSE){
  var2 <- c("Trend.p.long.", "Detectable...change.long.", "First.Year.long.", "Last.Year.long.",
            "No.of.Years.long.", "Trend.p.short.", "Detectable...change.short.",
            "First.Year.short.", "Last.Year.short.", "No.of.Years.short.",
            "Trends.2016.old")
  
  var2 %in% colnames(results_last_year)
  which(colnames(results_last_year) %in% var2)
}

data_xl_b <- data_xl  # backup
# data_xl <- data_xl_b

data_xl <- safe_left_join(
  data_xl, 
  results_last_year[,c(var1, "Trends.2016.old")], 
  by = var1, 
  check = "V")

cat("\nNumber of columns:", ncol(data_xl), "\n") # 142



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 26. Save result so far as text + RDA file ----
#
# __Note__: This file isn't actually used later in the process - we use the one with less-thans (below)
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


library(data.table)
fwrite(data_xl, 
       file = paste0("Big_excel_table/Data_xl_ver", version_no, ".csv"), 
       quote = FALSE, sep = ";", dec = ",")
# Warning: it turns out that fwrite messes up encoding


saveRDS(data_xl, 
        file = paste0("Big_excel_table/Data_xl_ver", version_no, ".rda")
        )



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 27. Add less-than columns (as TRUE/FALSE) ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

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
  message("Lack of match in key variables!")
  message("################################")
} else {
  cat("\nAll key variables matches\n")
}


# Intersperse empty columns 
# colnames(data_lessthans)
extra_cols <- matrix(NA, nrow(data_lessthans), sum(isnum)) %>% as.data.frame()
colnames(extra_cols) <- paste0(colnames(data_lessthans)[isnum], "x")
# str(extra_cols)
data_lessthans2 <- cbind(data_lessthans, extra_cols)
# dput(sort(colnames(data_lessthans2)))

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
  cat("\nLess-than columns added\n")
} else {
  cat("\nLess-than columns NOT added!\n")
}

#
# Checks
#
# data_xl %>% filter(PARAM %in% "VDSI" & !is.na(Yr_2017)) %>% select(STATION_CODE, Yr_2013:Yr_2017)
# data_xl_lessthans %>% filter(PARAM %in% "VDSI" & !is.na(Yr_2017)) %>% select(STATION_CODE, Yr_2013:Yr_2017)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 27b. Add Class for second last year (2017) ----
#
# Modified from 11
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

colnumber_value <- which(colnames(data_xl) == paste0("Yr_", last_year-1)); colnumber_value
value_secondlast <- data_xl[,colnumber_value]

colnumber_lessthan <- which(colnames(data_lessthans) == paste0("Lt_", last_year-1)); colnumber_lessthan
lessthan_lastyear <- data_xl[,colnumber_value]

# Less-thans are set a tad lower, to be put in the lower class
sel <- !is.na(lessthan_lastyear) & lessthan_lastyear
value_secondlast[sel] <- value_secondlast[sel] - 0.00001

# Just to check that we get the correct classes, i.e., if the conc. is on the limit, we get the upper class (using right = FALSE)
check_classes <- cut(value_secondlast/data_lessthans$Q95, breaks = c(-999999,1,2,5,10,20,999999), right = FALSE)
levels(check_classes)

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

cat("\nNumber of columns:", ncol(data_xl_lessthans), "\n") # 219


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 28. Change a couple of sum PARAM names ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

sel <- data_xl_lessthans$PARAM %in% "KPAH"; sum(sel)
data_xl_lessthans$PARAM[sel] <- "PK_S"

sel <- data_xl_lessthans$PARAM %in% "PAH16"; sum(sel)
data_xl_lessthans$PARAM[sel] <- "PAHSS"


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 29. Save 'data_xl_lessthans' as text file ----    
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

fn <- paste0("Data_xl_lessthans_ver", version_no, ".csv")
fn_full <- paste0("Big_excel_table/", fn)

#
# csv
#
if (fn %in% dir("Big_excel_table")){
  cat("File", fn, "already exists! File will not be overwritten.")
} else {
  # Save CSV
  write.csv2(data_xl_lessthans, file = fn_full, quote = FALSE, na = "", row.names = FALSE)
  cat("File", sQuote(fn), "written to folder 'Big_Excel_table'\n")
  # Save RDS
  saveRDS(data_xl_lessthans, sub("csv", "rds", fn_full))
}

#
# rds
#
saveRDS(data_xl, 
        file = paste0("Big_excel_table/Data_xl_ver", version_no, ".rda")
)


# For checking (old) files:
if (FALSE){
  fn <- paste0("Big_excel_table/Data_xl_lessthans_ver01.csv")
  check <- read.csv2(file = fn)
  colnames(check)
}


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 30. Excel procedure ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# _The rest of the procedure is done in Excel, after this file has been written:_    
# * Open in Excel and Save As Excel Macro-enabled workbook (.xlsm)
# * Open **"Macros2.xlsm"**, goto Developer:Visual Basic (or View:Macros:Edit Macros), and copy-paste all 
#     the code from _Macros2.xlsm to your excel file. You can then close "Macros2.xlsm".
# * Go to Developer:Macros (or View:Macros) and run this macro:
#     + Format_Cells_Lessthan_Shade_EQS - ca. 5 minutes
#       (this combines Format_Cells_Lessthan, Format_Cells_Shade and Format_EQS_Color)
# * Then  run these (for each, choose one cell in the column to perform the macro on)
#     + Set_character_1_and_3_to_Wingdings - shall be run on two columns, Trend.2018 and Trend.2017
#     + Set_character_1_3_8_and_10_to_Wingdings - select one column, TREND_CHANGE 
# 
# 
# * Columns (In Excel) - 2018 VERSION (data up to 2017)
# + W = Yr1980
# + Y = Yr1981
# + CS = Yr2017
# + EJ = Trends.2016.old
# + EK = Lt_1981
# + HE = Lt_2017
# + Rows = 2:29547

# * Columns (In Excel) - 2019 VERSION (data up to 2018)
# + W = Yr1980
# + Y = Yr1981
# + CU = Yr2018
# + EL = Trends.2016.old
# + EM = Lt_1981
# + HI = Lt_2017
# + Rows = 2:25384


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Appendix: checking/plotting ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# A. How to plot from file ----
#
#   1. time series plot
#   2. map plot
#   3. text file for Kepler
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

if (FALSE){
  
  dat <- readRDS("Big_excel_table/Data_xl_lessthans_ver02.rds")
  
  selcol_values <- grep("^Yr_", colnames(dat), value = TRUE)
  selcol_lt <- grep("^Lt_", colnames(dat), value = TRUE)
  
  selcol_values <- c(
    "PARAM", "STATION_CODE", "LATIN_NAME", "TISSUE_NAME", "Basis",
    selcol_values)
  
  selcol_lt <- c(
    "PARAM", "STATION_CODE", "LATIN_NAME", "TISSUE_NAME", "Basis",
    selcol_lt)
  
  dat_values <- dat[selcol_values] %>%
    as_tibble() %>%
    gather("MYEAR", "Median", -(PARAM:Basis)) %>%
    filter(!is.na(Median)) %>%
    mutate(MYEAR = as.numeric(sub("Yr_", "", MYEAR)))
  # dat_values

  # Add coordinates + station names
  data_stations <- readxl::read_excel("Input_data/Kartbase.xlsx")
  
  data_stations <- data_stations %>%
    select(stasjonskode, `catch LAT__1`, `catch LONG__1`, stasjonsnavn) %>%
    rename(STATION_CODE = stasjonskode,
           Lat = `catch LAT__1`, Long = `catch LONG__1`, 
           STATION_NAME = stasjonsnavn) %>%
    filter(!is.na(STATION_CODE))
  
  dat_values <- dat_values %>%
    safe_left_join(data_stations)
  
  # Simple time series plot (one station)
  
  library(ggplot2)
  
  dat_values %>%
    filter(
      PARAM %in% "CB118" & 
        STATION_CODE %in% "30B" &
        Basis %in% "WW" &
        TISSUE_NAME %in% "Liver"
    ) %>%
    ggplot(aes(MYEAR, Median, color = TISSUE_NAME)) +
    geom_point() +
    geom_smooth(method = "lm")
  
  # Simple 'map' plot (one year)
  
  dat_values %>%
    filter(
      PARAM %in% "CB118" & 
        Basis %in% "WW" &
        TISSUE_NAME %in% "Liver" & 
        MYEAR == 2018
    ) %>%
    ggplot(aes(Long, Lat, Median, color = log10(Median), size = log10(Median))) +
    geom_point()
  
  #
  # Check in https://kepler.gl/demo 
  #
  # The code below writes a text file
  # In kepler, hit "new data" and drag'n drop the file
  fn <- c("C:/Data/Temp/Data_CB118.csv")
  dat_values %>%
    filter(
      PARAM %in% "CB118" & 
        Basis %in% "WW" &
        TISSUE_NAME %in% "Liver"
    ) %>%
    mutate(Time = paste0(MYEAR, "-07-01 00:00:00")) %>%
    select(Long, Lat, Time, MYEAR, Median) %>%
  write.csv(fn)
  
}


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# B. Comparing with last year's file ----  
#
# Why 19000 rows this year, when it was 29000 last year
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# This year
if (FALSE){
  
  # Big excel data
  fn <- "../Milkys_2018/Big_excel_table/Data_xl_lessthans_ver14_greenred.xlsm"
  check1 <- readxl::read_excel(fn)
  
  fn <- "Big_excel_table/Data_xl_lessthans_ver02.csv"
  check2 <- read.csv2(file = fn, stringsAsFactors = FALSE)

  nrow(check1)
  nrow(check2)
  
  head(colnames(check1), 25)
  head(colnames(check2), 25)
  
  # Medians, from this year
  data_med <- readRDS(file = "Data/11_data_med_updated_02.rds")
  
  # Make combinations of key columns, for each year
  comb1 <- check1 %>%
    count(PARAM, STATION_CODE, LATIN_NAME, TISSUE_NAME, Basis) %>%
    rename(n_2018 = n)
  table(comb1$n_2018)

  comb2 <- check2 %>%
    count(PARAM, STATION_CODE, LATIN_NAME, TISSUE_NAME, Basis) %>%
    rename(n_2019 = n)
  table(comb2$n_2019)    # SOme eider duck data are twice - see below

  # Change tissue names of 2018 version
  xtabs(~addNA(TISSUE_NAME), comb1)
  xtabs(~addNA(TISSUE_NAME), comb2)

  comb1 <- comb1 %>%
    mutate(TISSUE_NAME = 
             case_when(TISSUE_NAME %in% "Blod" ~ "Blood",
                       TISSUE_NAME %in% "Egg homogenate of yolk and albumin" ~ "Egg",
                       TISSUE_NAME %in% "Galle" ~ "Bile",
                       TISSUE_NAME %in% "Muskel" ~ "Muscle",
                       TISSUE_NAME %in% "Lever" ~ "Liver",
                       TISSUE_NAME %in% "WO" ~ "Whole organism",
                       TRUE ~ TISSUE_NAME)
    )
  
  # Join tables
  comb <- full_join(comb1, comb2) %>%
    mutate(Missing_2019 = n_2018 == 1 & is.na(n_2019))
  nrow(comb)   # 30083
  
  comb %>%
    count(n_2018, n_2019)  # 2019 lacking 11191
  
  # Basis %in% "DW_est" 
  comb %>%
    filter(Basis %in% "DW_est") %>%
    count(n_2018, n_2019)    # 6680
  
  # Excluding Basis "DW_est"
  comb %>%
    filter(!Basis %in% "DW_est") %>%
    count(n_2018, n_2019)    # Still 4511
  
  #
  # Parameters
  #
  df_par <- comb %>%
    filter(!Basis %in% "DW_est") %>%
    count(PARAM, Missing_2019) %>%
    spread(Missing_2019, n) %>%

  df_par %>% View("PARAM")
  # 2019 lacks a few records of many parameters, many records (ca 50%) for CB_S7
  # Completely missing stations: 1726 records
  df_par %>% filter(is.na(`FALSE`)) %>% View("Par missing")
  df_par %>% filter(is.na(`FALSE`)) %>% pull(`TRUE`) %>% sum()  # 623
  df_par %>% filter(is.na(`FALSE`)) %>% pull(PARAM) %>% dput()  
  # "BDE205", "BEP", "BLOPR", "CYP1A", "DBTC1", "DBTC2", "DBTC3", 
  # "NAPC1", "NAPC2", "NAPC3", "PAC1", "PAC2", "PAC3", "PER"
  
  #
  # Stations
  #
  df_stat <- comb %>%
    filter(!Basis %in% "DW_est") %>%
    count(STATION_CODE, Missing_2019) %>%
    spread(Missing_2019, n)
  df_stat %>% View("STATION_CODE")
  # 2019 lacks a few records of many stations

  # Completely missing stations: 1726 records
  df_stat %>% filter(is.na(`FALSE`)) %>% View("Station missing")
  df_stat %>% filter(is.na(`FALSE`)) %>% pull(`TRUE`) %>% sum()  # 1726
  df_stat %>% filter(is.na(`FALSE`)) %>% pull(STATION_CODE) %>% dput()  
  # 2019 lacks all of the following:
  # "15F", "227A2", "67B", "67F", "B3B", "I023", "I024", "I201", 
  # "I205", "I243", "I713", "I912", "I913" 
  
  # 15F: last year 2011
  # 227A2: last year 2011

  which(check1$STATION_CODE == "15F")
  which(check2$STATION_CODE == "15F")
  
  #
  # By tissue: few lacking liver
  #
  comb %>%
    filter(!Basis %in% "DW_est") %>%
    count(TISSUE_NAME, Missing_2019) %>%
    spread(Missing_2019, n) %>%
    View("TISSUE_NAME")
  
  #
  # Basis - many DWa + FBa lacking
  #
  comb %>%
    filter(!Basis %in% "DW_est") %>%
    count(Basis, Missing_2019) %>%
    spread(Missing_2019, n) %>%
    View("Basis")

  #
  # Add info to comb
  #
  df_par <- df_par %>%
    mutate(Lack_par = case_when(
      is.na(`FALSE`) ~ "Lack all",
      `TRUE`/`FALSE` > 0.5 ~ "Lack many", 
      `TRUE`/`FALSE` < 0.5 ~ "Lack few"
    ))
  
  df_stat <- df_stat %>%
    mutate(Lack_stat = case_when(
      is.na(`FALSE`) ~ "Lack all",
      `TRUE`/`FALSE` > 0.5 ~ "Lack many", 
      `TRUE`/`FALSE` < 0.5 ~ "Lack few"
    ))

  # add  
  comb <- comb %>%
    left_join(df_par %>% select(PARAM, Lack_par)) %>%
    left_join(df_stat %>% select(STATION_CODE, Lack_stat))
  
  # check
  df <- comb %>% 
    filter(is.na(n_2019)) %>%
    filter(!Basis %in% "DW_est") %>%
    filter(!Lack_par %in% "Lack all") %>%
    filter(!Lack_stat %in% "Lack all")
  nrow(df)    # 2269
  df %>% View("comb")
  # Examples
  # AG 51A - last year 2011
  # ACNE I306 - last year 2011
  
  
  # NOTE: eider duck egg data are twice
  # For some reason, 2017 data in one row, 2018 data in the other
  table(comb2$n_2019)
  comb2 %>% 
    filter(n_2019 > 1) %>% View("dubl")
  comb2 %>% 
    filter(n > 1) %>%
    count(LATIN_NAME)
  
}

if (FALSE){
  version_previous <- "06"
  data_xl_prev <- readRDS(paste0("Big_excel_table/Data_xl_ver", version_previous, ".rda"))
  
  param_curr <- data_xl %>% count(PARAM)
  param_prev <- data_xl_prev %>% count(PARAM)
  data_xl %>%
    count(PARAM) %>%
    safe_anti_join(param_prev, by = c("PARAM", "n"))
  data_xl %>%
    count(PARAM) %>%
    safe_anti_join(param_prev, by = c("PARAM"))
  data_xl_prev %>%
    count(PARAM) %>%
    safe_anti_join(param_curr, by = c("PARAM"))

  data_new <- data_xl %>%
    filter(PARAM %in% c("TPTIN","VDSI/Intersex"))
  
}



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# C. Test with Kepler ----  
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


if (FALSE){
  dat <- readRDS("Big_excel_table/Data_xl_lessthans_ver02.rds")
  data_stations <- readxl::read_excel("Input_data/Kartbase.xlsx")
  
  data_stations <- data_stations %>%
    select(stasjonskode, `catch LAT__1`, `catch LONG__1`, stasjonsnavn) %>%
    rename(STATION_CODE = stasjonskode,
           Lat = `catch LAT__1`, Long = `catch LONG__1`, 
           STATION_NAME = stasjonsnavn) %>%
    filter(!is.na(STATION_CODE))
  
  yrc <- colnames(dat) 
  dat <- dat %>%
    
  
}