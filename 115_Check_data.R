
# Check last year's data, downloaded from Nivabase
dat_lastyear <- readRDS("Input_data/80_df_2023_notstandard_2024-09-23_20h06m.rds")

# Check all years' data, at different stages
dat_all <- readRDS("Data/101_dat_new_2024-09-23.rds")
dat_all <- readRDS("Data/103_data_updated_2024-09-23.rds")
dat_all <- readRDS("Data/105_data_with_uncertainty_2024-09-23.rds")

# table: STATION_CODE + MYEAR
xtabs(~STATION_CODE + MYEAR, subset(dat_lastyear, MYEAR > 2018))
xtabs(~STATION_CODE + MYEAR, subset(dat_all, MYEAR > 2018))

# table: PARAM + MYEAR for eider duck  
xtabs(~NAME + MYEAR, subset(dat_lastyear, MYEAR > 2018 & STATION_CODE == "19N"))
xtabs(~PARAM + MYEAR, subset(dat_all, MYEAR > 2018 & STATION_CODE == "19N"))

# test plot some substances, eider duck 
substance <- "D5"
substance <- "HG"
substance <- "PFOS"
substance <- "BDE99"
substances <- c("BDE47", "BDE99", "BDE100")
ggplot(data = subset(dat_all, MYEAR > 2018 & STATION_CODE == "19N" & PARAM %in% substances),
       aes(MYEAR, VALUE_WW, color = (FLAG1 %in% "<"))) +
  geom_jitter(width = 0.1) +
  facet_grid(rows = vars(PARAM), cols = vars(TISSUE_NAME)) +
  labs(title = paste("Eider duck:", paste(substances, collapse = ", ")))
  
