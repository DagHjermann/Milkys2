
library2(dbplyr)
library2(dplyr)
library2(lubridate)
library2(ggplot2)
library(ggeasy)
library2(stringr)


#
# Run 080 parts 1-4 first (through "set up queries")!
#

#
# Checking SCCP and MCCP ----
#


# . Test case, Lofoten cod data ----
# all data for 2022   
dat_lofoten_2022 <- dat_2 %>%
  filter(MYEAR == 2022 & STATION_CODE == "98B1") %>%
  collect()
xtabs(~NAME, dat_lofoten_2022)


# . Get all SCCP methods ----

# First try:
df_meth <- methods %>%
  filter(NAME %in% c("SCCP eksl. LOQ", "SCCP inkl. LOQ", "SCCP")) %>%
  collect()

# Even better:
df_meth <- methods %>%
  filter(NAME %like% '%SCCP%') %>%
  collect()
# from
# https://stackoverflow.com/a/50239932

method_ids <- pull(df_meth, METHOD_ID)

# . All SCCP for Lofoten cod ----
# check BIOTA_CHEMISTRY_VALUES to see who 

# redefine measurements by adding ENTERED_BY, ENTERED_DATE
measurements <- tbl(con, in_schema("NIVADATABASE", "BIOTA_CHEMISTRY_VALUES")) %>%
  select(SAMPLE_ID, METHOD_ID, VALUE_ID, VALUE, FLAG1, 
         DETECTION_LIMIT, UNCERTAINTY, QUANTIFICATION_LIMIT, 
         ENTERED_BY, ENTERED_DATE)

# redefine dat2 from script 080, adding ENTERED_BY, ENTERED_DATE 
dat_2 <- dat_1 %>%
  distinct(
    # Main data
    PROJECT_NAME, STATION_CODE, STATION_NAME, LATIN_NAME, MYEAR, TISSUE_NAME, SAMPLE_NO, REPNO, 
    NAME, VALUE, FLAG1, UNIT, 
    # extra time info
    # DATE_CAUGHT, YEAR, MONTH,
    # extra sample info (note: LABWARE_TEXT_ID not included as there can be two or more TEXT_ID for a single sample)
    REMARK_sample, 
    # extra chemical methiod info
    LABORATORY, METHOD_REF, DETECTION_LIMIT, UNCERTAINTY, QUANTIFICATION_LIMIT, 
    # IDs
    PROJECT_ID, STATION_ID, TAXONOMY_CODE_ID, MATRIX_ID, SAMPLE_ID, TISSUE_ID, METHOD_ID, VALUE_ID, 
    ENTERED_BY, ENTERED_DATE)

# then run


dat_lofoten_sccp <- dat_2 %>%
  filter(STATION_CODE == "98B1" & METHOD_ID %in% method_ids) %>%
  collect()

xtabs(~MYEAR + NAME, dat_lofoten_sccp)
xtabs(~MYEAR + ENTERED_BY + NAME, dat_lofoten_sccp)


#
# . SCCP in water ----
#

test_water <- tbl(con, in_schema("NIVADATABASE", "WATER_CHEMISTRY_VALUES")) %>%
  filter(METHOD_ID %in% method_ids) %>%
  collect()


#
# test of overviews ----
#

#
# . all data from a given year ----
# 
measurement_year <- 2023
dat_overview <- dat_2 %>%
  filter(MYEAR == measurement_year) %>%
  count(NAME, STATION_CODE) %>%
  collect()

ggplot(dat_overview, aes(NAME, STATION_CODE, fill = n)) +
  geom_tile() +
  scale_fill_viridis_c() +
  easy_rotate_labels(which = "x", angle = -45)


#
# . all data from a given substance ----
# 

param <- "CB118"
param_wild <- "%CB118&"

method_ids <- c(
  methods %>%
    filter(NAME %like% '%CB118%') %>%
    pull(METHOD_ID),
  methods %>%
    filter(NAME %like% '%CB 118%') %>%
    pull(METHOD_ID)
)

# get all data
dat_overview_all <- dat_2 %>%
  filter(METHOD_ID %in% method_ids) %>%
  count(NAME, STATION_CODE, MYEAR) %>%
  collect() 

# add variables for filtering time series
dat_overview <- dat_overview_all %>%
  group_by(STATION_CODE) %>%
  mutate(
    n_year = n(),
    last_year = max(MYEAR)) %>%
  ungroup()

# all stations
ggplot(dat_overview, 
       aes(MYEAR, STATION_CODE, fill = n)) +
  geom_tile() +
  scale_fill_viridis_c() +
  easy_rotate_labels(which = "x", angle = -45)

# all stations with >= 3 years of data
ggplot(dat_overview %>% filter(n_year >= 3), 
       aes(MYEAR, STATION_CODE, fill = n)) +
  geom_tile() +
  scale_fill_viridis_c() +
  easy_rotate_labels(which = "x", angle = -45)

# all stations with >= 3 years of data and data since 2015
ggplot(dat_overview %>% filter(n_year >= 3 & last_year >= 2015), 
       aes(MYEAR, STATION_CODE, fill = n)) +
  geom_tile() +
  scale_fill_viridis_c() +
  easy_rotate_labels(which = "x", angle = -45)


