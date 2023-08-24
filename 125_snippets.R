
xtabs(~MYEAR + LATIN_NAME, dat_all %>% filter(PARAM == "PFOS"))

xtabs(~MYEAR + LATIN_NAME, dat_all %>% filter(PARAM == "PFOA"))
xtabs(~MYEAR + LATIN_NAME, dat_all %>% filter(PARAM == "PFOSA"))

xtabs(~LATIN_NAME, dat_series %>% filter(PARAM == "PFOS"))
xtabs(~LATIN_NAME, dat_series %>% filter(PARAM == "PFOS"))
xtabs(~LATIN_NAME, dat_series %>% filter(PARAM == "PFOS"))

xtabs(~LATIN_NAME, dat_series_trend %>% filter(PARAM == "PFOA"))
xtabs(~LATIN_NAME, dat_series_trend %>% filter(PARAM == "PFOSA"))

xtabs(~LATIN_NAME, dat_all_prep1 %>% filter(PARAM == "PFOSA"))
xtabs(~LATIN_NAME, dat_all_prep1 %>% filter(PARAM == "PFOA"))

xtabs(~LATIN_NAME, dat_all_prep3 %>% filter(PARAM == "PFOSA"))
xtabs(~LATIN_NAME, dat_all_prep3 %>% filter(PARAM == "PFOA"))

xtabs(~LATIN_NAME, dat_all_prep3 %>% filter(PARAM == "PFOSA" & MYEAR == 2021))
xtabs(~LATIN_NAME, dat_all_prep3 %>% filter(PARAM == "PFOA" & MYEAR == 2021))

dat_all_prep3 %>% filter(PARAM == "PFOSA" & MYEAR == 2021) %>% View()

xtabs(is.na(FLAG1) ~ MYEAR + STATION_CODE, dat_all_prep3 %>% filter(PARAM == "PFOSA"))
xtabs(is.na(FLAG1) ~ MYEAR + STATION_CODE, dat_all_prep3 %>% filter(PARAM == "PFOA"))

xtabs(~LATIN_NAME, dat_series_1 %>% filter(PARAM == "PFOSA"))
xtabs(~LATIN_NAME, dat_series_1 %>% filter(PARAM == "PFOA"))

xtabs(~LATIN_NAME, dat_series %>% filter(PARAM == "PFOSA"))
xtabs(~LATIN_NAME, dat_series %>% filter(PARAM == "PFOA"))

dat_series_1 %>%
  filter(Last_year_overall >= last_year & N_years_10yr > 0 & PARAM == "PFOSA") %>% 
  xtabs(~LATIN_NAME, .)
dat_series_1 %>%
  filter(Last_year_overall >= last_year & N_years_10yr > 0 & PARAM == "PFOA") %>% 
  xtabs(~LATIN_NAME, .)

dat_series_1 %>%
  filter(Last_year_overall >= last_year & PARAM == "PFOSA") %>% 
  xtabs(~LATIN_NAME, .)
dat_series_1 %>%
  filter(Last_year_overall >= last_year & PARAM == "PFOA") %>% 
  xtabs(~LATIN_NAME, .)

dat_series_1 %>%
  filter(N_years_10yr > 0 & PARAM == "PFOSA") %>% 
  xtabs(~LATIN_NAME, .)
dat_series_1 %>%
  filter(N_years_10yr > 0 & PARAM == "PFOA") %>% 
  xtabs(~LATIN_NAME, .)

