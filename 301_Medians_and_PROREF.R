
# Calculates all medians (as length-adjusted values changes for all years,
#   not only 2018)
# Also adds PROREF. NOTE: No PROREF values are calculated, just ues old ones
#
# This script REPLACES '11_Add_2019_medians_to_old_medians.R'
#

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# NOTE: If new parameters have been added, check and add to section 9
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o





#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#  
# 1. Libraries ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

library(dplyr)
# library(purrr)
library(tidyr)
library(data.table)
library(ggplot2)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 2. Data ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

data_all <- readRDS("Data/10_data_all.RData")

# Check "I0" stations
if (FALSE){
  data_all %>%
    filter(substr(STATION_CODE,1,2) %in% c("I0")) %>%
    xtabs(~MYEAR + STATION_CODE, .)
}

#
# PROREF
#

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

#
# Big Excel sheet

df_lastyear <- read.csv2("../Milkys_2018/Input_data/13b8_data_xl_ver26_firstcols.csv")
df_lastyear2 <- readxl::read_excel("../Milkys_2018/Input_data/Trendtabell_20160919_Komplett_v4_GSE med samletabell.xlsx", sheet = "RAW_Data")

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 3. Fixing 2018 data ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

### . Fixing station 227G2 (shall count as 227G) ----
sel <- data_all$STATION_CODE %in% "227G2"; sum(sel)
# xtabs(~MYEAR, data_all[sel,])
data_all$STATION_CODE[sel] <- "227G"

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#
#
# . Check LATIN_NAME + STATION_NAME ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#

#### Check 2018 data 
xtabs(~addNA(LATIN_NAME), data_all)
xtabs(~addNA(STATION_CODE), data_all)


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# . Check parameters with NA ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# df %>% 
#   filter(UNIT %in% "NA, %")
# 
# df %>% 
#   filter(UNIT %in% "NA, UG_P_KG")


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 4. Calculate medians ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Convert to data.table format
data_all_dt <- as.data.table(data_all)

# Make median of quantitative columns
cols_to_median <- c("DRYWT","FAT_PERC","VALUE_WW","VALUE_DW","VALUE_FB","VALUE_WWa","VALUE_DWa","VALUE_FBa")
t0 <- Sys.time()
data_med_1 <- data_all_dt[,
                          lapply(.SD, median, na.rm=TRUE), 
                          by = .(MYEAR, STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM, UNIT), 
                          .SDcols = cols_to_median]
Sys.time() - t0  # 0.07100201 secs  (!)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 5. Calculate N, Det_limit and Over_LOQ ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


t0 <- Sys.time()
data_med_2 <- data_all_dt[,
                          .(.N, Det_limit = median(VALUE_WW[!is.na(FLAG1)]), Over_LOQ = sum(is.na(FLAG1))), 
                          by = .(MYEAR, STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM, UNIT)
                          ]
Sys.time() - t0   # 1.9 seconds


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 6. Join to median data set  ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

colnames(data_med_1)[1:6]

cat("Index columns are the same if all following numbers are 1:\n")
check <- apply(data_med_1[,1:5] == data_med_2[,1:5], 2, mean) == rep(1,5)
if (sum(check) == 5){
  cat("Index columns 1-5 are the same\n")
} else {
  cat("Index columns 1-5 are NOT the same, check before proceeding!\n")
}

# Also check column 6, UNIT (contains NAs)
check <- identical(data_med_1[[6]], data_med_2[[6]])
if (check){
  cat("Index columns 6 are the same, data can be joined by column\n")
} else {
  cat("Index columns 6 are NOT the same, check before proceeding!\n")
}


# Join data
data_med <- cbind(data_med_1, data_med_2[,-(1:6)])    # Add columns of data_med_2, except columns 1-5

nrow(data_all)  # 486483
nrow(data_med)  #  81808


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
## 7. Fix 71G ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Special case: 71G: Nucella until 2009, Littorina from 2017
# We sort out that below
data_med %>%
  filter(STATION_CODE == "71G") %>%
  count(STATION_CODE, LATIN_NAME, MYEAR)

# Change it to 'N. lapillus / L. littorea'
sel <- data_med$STATION_CODE %in% "71G"; sum(sel)
data_med$LATIN_NAME[sel] <- "N. lapillus / L. littorea"

# Also change parameter (for trends and the big table)
# Note: must be changed to VDSI for the overview tables
sel <- with(data_med, STATION_CODE %in% "71G" & PARAM %in% c("VDSI", "Intersex")); sum(sel)
data_med$PARAM[sel] <- "VDSI/Intersex"


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 8. Fix big Excel sheet ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#
# Col names 
#
cols <- c("Parameter Code", "Parameter Name", "IUPAC", "CAS", "Component Name", 
          "Substance Group", "Unit", "Station Code", "Station Name", "Area", 
          "County", "Water Region", "VannforekomstID", "VAnnforekomstNavn")
df_lastyear2 <- df_lastyear2[,cols]

# Check that order is the same
colnames(df_lastyear); cat("\n")
colnames(df_lastyear2)

# Change column names
colnames(df_lastyear2) <- colnames(df_lastyear)

# Also change these
sel <- df_lastyear2$PARAM %in% "FETT"; sum(sel)
df_lastyear2$PARAM[sel] <- "Fett"

sel <- df_lastyear2$PARAM %in% "PAHSS"; sum(sel)
df_lastyear2$PARAM[sel] <- "PAH16"

sel <- df_lastyear2$PARAM %in% "PK_S"; sum(sel)
df_lastyear2$PARAM[sel] <- "KPAH"


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 9. Select parameters occuring in the big Excel sheet ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Select parameters
# Also add D4, D5 and D6


sel_by_parameter <- 
  data_med$PARAM %in% c(unique(df_lastyear2$PARAM),
                        "D4", "D5", "D6",            # Siloxans
                        "VDSI/Intersex",             # Station 71G
                        "DDC_PA", "DDC_PS", "DDC_BBF", "DDC_DBF", "DDC_ANT", "HCTBPH")  # Chlorinated flame retardants
                                                                                        #   (Dechloran plus, etc.)


# Not selected
sum(!sel_by_parameter)       # 7290
mean(!sel_by_parameter)*100  # 8.63 %

# Parameters NOT selected
# paste(unique(data_med_old$PARAM[!sel_by_parameter]), collapse = ", ")

# Keep only the selected parameters
data_med_updated <- data_med[sel_by_parameter,]
nrow(data_med_updated)  # 73923 / 71572


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 10. Save values without PROREF ----
#
# Used in script 12
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

if (TRUE){
  saveRDS(data_med_updated, "Data/11_data_med_without_PROREF.rds")
}

# Read from file: 
if (FALSE){
  data_med_updated <- readRDS("Data/11_data_med_without_PROREF.rds")
}



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 11. Check PROREF data ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Check if there is just one PARAM, LATIN_NAME, TISSUE_NAME, UNIT, Basis
check1 <- proref_updated02 %>%
  count(PARAM, LATIN_NAME, TISSUE_NAME, Basis)
sum(check1$n > 1)  # 0

# Check if Q95 is unique for a given PARAM, LATIN_NAME, TISSUE_NAME, UNIT, Basis
check2 <- proref_updated02 %>%
  group_by(PARAM, LATIN_NAME, TISSUE_NAME, Basis) %>%
  summarize(NQ95 = n_distinct(Q95))
sum(check2$NQ95 > 1)  # 0

table(check2$LATIN_NAME)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 12. Add "VDSI/Intersex" to PROREF data ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Pick one VDSI line and change it
sel <- proref_updated02$PARAM == "VDSI" & proref_updated02$Basis == "WW"; sum(sel)
proref_to_add <- proref_updated02[sel,]
proref_to_add$PARAM <- "VDSI/Intersex"

# Add 
proref_updated03 <- bind_rows(
  proref_updated02,
  proref_to_add
)

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 13. Add PROREF data to medians ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

data_med_updated_02 <- data_med_updated %>%
  tidyr::gather("Basis", "Value", 
                VALUE_WW, VALUE_DW, VALUE_FB, VALUE_WWa, VALUE_DWa, VALUE_FBa) %>%
  mutate(Basis = stringr::str_sub(Basis, start = 7))
nrow(data_med_updated_02)   # 429432

library(safejoin)
data_med_updated_02 <- data_med_updated_02 %>%
  safe_left_join(proref_updated03, check = "V"
  )


nrow(data_med_updated_02)  # 448680


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# 14. Save ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#
# Make backup 
#
file.copy("Data/11_data_med_updated_02.rds", 
          paste0("Data/11_data_med_updated_02_back", substr(Sys.time(),1,10),".rds"))

#
# Save
#
saveRDS(data_med_updated_02, file = "Data/11_data_med_updated_02.rds")

# For reading:
# data_med_updated_02 <- readRDS(file = "Data/11_data_med_updated_02.rds")

if (FALSE){
  data_med_updated_02 %>%
    filter(STATION_CODE %in% "19N" & PARAM %in% "PFAS" & Basis == "WW")
}
  

