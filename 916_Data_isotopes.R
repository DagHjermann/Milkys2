
dat <- readRDS("Data/109_adjusted_data_2022-09-23.rds")

dat %>% 
  filter(MYEAR == 2021) %>%
  xtabs(~PARAM, .)

dat %>% 
  filter(MYEAR == 2021 & PARAM %in% c("% C", "% N", "C/N", "Delta13C", "Delta15N")) %>%
  select(PARAM, STATION_CODE, LATIN_NAME, TISSUE_NAME, SAMPLE_NO2, VALUE_WW) %>%
  tidyr::pivot_wider(names_from = PARAM, values_from = VALUE_WW) %>%
  writexl::write_xlsx("Data/109_adjusted_data_2022-09-23_isotopes.xlsx")
