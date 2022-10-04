


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# get_data_medians
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


get_data_medians <- function(param, species, tissue, basis, include_year,
                             filename_110 = "Data/110_mediandata_updated_2022-09-01.rds",
                             filename_lookup_substancegroups = "Input_data/Lookup_tables/Lookup table - substance groups.csv",
                             filename_lookup_stations= "Input_data/Lookup_tables/Lookup_stationorder.csv",
                             filename_lookup_eqs = "Input_data/Lookup_tables/Lookup_EQS_limits.csv",
                             filename_lookup_proref = "Input_data/Lookup_tables/Lookup_proref.csv"){
  
  indexvars <- c("PARAM", "STATION_CODE", "TISSUE_NAME", "LATIN_NAME", "Basis")
  
  # due to code for 'lookup_eqs'
  if (length(species) > 1)
    stop("Works only for one 'species'")  
  if (length(basis) > 1)
    stop("Works only for one 'basis'")
  
  # Parameter groups
  lookup_paramgroup <- read.csv(filename_lookup_substancegroups) %>%
    select(PARAM, Parameter.Name)
  
  # Stations
  lookup_stations <- read.csv(filename_lookup_stations) 
  
  # EQS and proref   
  lookup_proref <- read.csv(filename_lookup_proref) %>%
    select(PARAM, LATIN_NAME, TISSUE_NAME, Basis, Proref)
  
  # For eider duck, we use the proref of cod
  if (species == "Somateria mollissima"){
    lookup_proref$LATIN_NAME <- "Somateria mollissima"
  }
  
  lookup_eqs <- read.csv(filename_lookup_eqs) %>%
    rename(EQS = Limit) %>%
    select(-Long_name, -Kommentar) %>%
    # We set all empty species cells to the s
    mutate(
      LATIN_NAME = case_when(
        is.na(LATIN_NAME) ~ species,
        TRUE ~ LATIN_NAME)
    )
  
  if (basis == "WWa"){
    # Add extra set of rows with WWa instead of WW
    lookup_eqs <- bind_rows(
      lookup_eqs,
      lookup_eqs %>% 
        mutate(
          Basis = case_when(
            Basis %in% "WW" ~ "WWa",
            TRUE ~ Basis)
        )
    )
  }

  
    
    # Raw data
  # dat_all <- readRDS(filename_109) %>%
  #   dplyr::filter(PARAM %in% param,
  #                 LATIN_NAME = species, 
  #                 TISSUE_NAME = tissue)
  
  # Medians
  if (basis != "WWa"){
  dat_medians_01 <- readRDS(filename_110) %>%
    dplyr::filter(PARAM %in% param,
                  LATIN_NAME %in% species, 
                  TISSUE_NAME %in% tissue,
                  Basis %in% basis)
  } else {
    # if Basis = WWa, we use WWa when available, otherwise we use WW
    dat_medians_wide <- readRDS(filename_110) %>%
      dplyr::filter(
        PARAM %in% param,
        LATIN_NAME %in% species, 
        TISSUE_NAME %in% tissue,
        Basis %in% c("WW", "WWa")) %>%
      select(MYEAR, STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM, UNIT, Basis, Over_LOQ, N_median, Value) %>%
      tidyr::pivot_wider(names_from = Basis, values_from = Value)
    
    dat_medians_01 <- dat_medians_wide %>%
      mutate(
        Value = case_when(
          !is.na(WWa) ~ WWa,
          is.na(WWa) ~ WW),
        Basis = case_when(
          !is.na(WWa) ~ "WWa",
          is.na(WWa) ~ "WW")
      ) %>%
      select(-WW, -WWa)
  }
  
  # Keep only series where the last year (include_year) is included
  dat_medians_01 <- dat_medians_01 %>% 
    group_by(STATION_CODE) %>%
    mutate(Series_last_year = max(MYEAR)) %>%
    filter(Series_last_year %in% include_year)
  
  dat_medians_02 <- dat_medians_01 %>%
    left_join(lookup_proref, by = c("PARAM", "TISSUE_NAME", "LATIN_NAME", "Basis")) 
  
  if (nrow(dat_medians_01) != nrow(dat_medians_02))
    warning("Number of rows changed when adding proref (get_data_medians)")
  
  dat_medians_03 <- dat_medians_02 %>%
    left_join(lookup_eqs, by = c("PARAM", "LATIN_NAME", "Basis")) 
  
  if (nrow(dat_medians_02) != nrow(dat_medians_03))
    warning("Number of rows changed when adding EQS (get_data_medians)")

  lookup_stations <- lookup_stations %>%
    filter(STATION_CODE %in% unique(dat_medians_03$STATION_CODE)) %>%
    mutate(Station = paste(STATION_CODE, Station_name)) %>%
    arrange(Order)
    
  dat_medians_04 <- dat_medians_03 %>%
    # Add station name
    left_join(
      lookup_stations %>% 
        select(STATION_CODE, Station_name, Station, Region),
      by = "STATION_CODE") %>%
    # Order by 'station_order
    mutate(
      Station = factor(
        paste(STATION_CODE, Station_name),
        levels = rev(lookup_stations$Station))
    )
      
  if (nrow(dat_medians_03) != nrow(dat_medians_04))
    warning("Number of rows changed when adding station (get_data_medians)")
  
  dat_medians_04 <- dat_medians_04 %>%
    mutate(
      Proref_ratio = Value/Proref,
      EQS_ratio = Value/EQS,
      Above_EQS = case_when(
        Value > EQS ~ "Over",
        Value <= EQS ~ "Under",
        TRUE ~ as.character(NA)),
      Prop_underLOQ = 1 - Over_LOQ/N_median,
      FLAG1 = case_when(
        Prop_underLOQ < 0.5 ~ as.character(NA),
        Prop_underLOQ >= 0.5 ~ "<"),
      LOQ_label = ifelse(Prop_underLOQ >= 0.5, "<", "")
    )
  
  # Add 'Proref_ratio_cut'  
  proref_max <- max(dat_medians_04$Proref_ratio, na.rm = TRUE)
  dat_medians_04 <- dat_medians_04 %>%
    mutate(
      Proref_ratio_cut = cut(Proref_ratio, breaks = c(0,0.5,0.75,0.9,1,2,5,10,20,100))
    )
  
  dat_medians_05 <- dat_medians_04 %>%
    filter(!is.na(Station)) %>%
    ungroup()
  
  if (nrow(dat_medians_05) < nrow(dat_medians_04))
    warning("Some stations deleted, probably not part of 'lookup_stations' (get_data_medians)")
  
  dat_medians_06 <- dat_medians_05 %>%
    left_join(lookup_paramgroup, by = "PARAM")
  if (nrow(dat_medians_06) != nrow(dat_medians_05))
    warning("Number of rows changed when adding Parameter.Name (get_data_medians)")
  
  dat_medians_06
  
}

if (FALSE){
  
  # debugonce(get_data_medians)
  check <- get_data_medians("HG", "Gadus morhua", "Muskel", "WWa", 2021)
  check %>% distinct(PARAM, STATION_CODE, LATIN_NAME, TISSUE_NAME, Basis, Station) %>% filter(STATION_CODE == "30B") %>% View("30B")
  check %>% filter(STATION_CODE == "30B") %>% View("30B")
  
}



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# get_data_raw
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


get_data_raw <- function(param, species, tissue, basis, include_year,
                         filename_109 = "Data/109_adjusted_data_2022-09-01.rds",
                         filename_lookup_substancegroups = "Input_data/Lookup_tables/Lookup table - substance groups.csv",
                         filename_lookup_stations= "Input_data/Lookup_tables/Lookup_stationorder.csv",
                         filename_lookup_eqs = "Input_data/Lookup_tables/Lookup_EQS_limits.csv",
                         filename_lookup_proref = "Input_data/Lookup_tables/Lookup_proref.csv"){
  
  indexvars <- c("PARAM", "STATION_CODE", "TISSUE_NAME", "LATIN_NAME", "Basis")
  
  # due to code for 'lookup_eqs'
  if (length(species) > 1)
    stop("Works only for one 'species'")  
  if (length(basis) > 1)
    stop("Works only for one 'basis'")
  
  # Parameter groups
  lookup_paramgroup <- read.csv(filename_lookup_substancegroups)
  
  # Stations
  lookup_stations <- read.csv(filename_lookup_stations)
  
  # EQS and proref   
  lookup_proref <- read.csv(filename_lookup_proref) %>%
    select(PARAM, LATIN_NAME, TISSUE_NAME, Basis, Proref)
  
  lookup_eqs <- read.csv(filename_lookup_eqs) %>%
    rename(EQS = Limit) %>%
    select(-Long_name, -Kommentar) %>%
    # We set all empty species cells to the s
    mutate(
      LATIN_NAME = case_when(
        is.na(LATIN_NAME) ~ species,
        TRUE ~ LATIN_NAME)
    )
  
  if (basis == "WWa"){
    # Add extra set of rows with WWa instead of WW
    lookup_eqs <- bind_rows(
      lookup_eqs,
      lookup_eqs %>% 
        mutate(
          Basis = case_when(
            Basis %in% "WW" ~ "WWa",
            TRUE ~ Basis)
        )
    )
  }
  
  # Raw data
  # dat_all <- readRDS(filename_109) %>%
  #   dplyr::filter(PARAM %in% param,
  #                 LATIN_NAME = species, 
  #                 TISSUE_NAME = tissue)
  
  # Medians
  dat_01 <- readRDS(filename_109) %>%
    dplyr::filter(PARAM %in% param,
                  LATIN_NAME %in% species, 
                  TISSUE_NAME %in% tissue) %>%
    group_by(STATION_CODE) %>%
    mutate(Series_last_year = max(MYEAR)) %>%
    filter(Series_last_year %in% include_year)
  
  if (basis != "WWa"){
    dat_01$Value <- dat_01[[paste0("VALUE_", basis)]] 
    dat_01$Basis <- basis
  } else {
    dat_01 <- dat_01 %>%
      mutate(
        Value = case_when(
          !is.na(VALUE_WWa) ~ VALUE_WWa,
          is.na(VALUE_WWa) ~ VALUE_WW),
        Basis = case_when(
          !is.na(VALUE_WWa) ~ "WWa",
          is.na(VALUE_WWa) ~ "WW")
        ) 
  }
  
  dat_02 <- dat_01 %>%
    left_join(lookup_proref, by = c("PARAM", "TISSUE_NAME", "LATIN_NAME", "Basis")) 
  
  if (nrow(dat_01) != nrow(dat_02))
    warning("Number of rows changed when adding proref (get_data_medians)")
  
  dat_03 <- dat_02 %>%
    left_join(lookup_eqs, by = c("PARAM", "LATIN_NAME", "Basis")) 
  
  if (nrow(dat_02) != nrow(dat_03))
    warning("Number of rows changed when adding EQS (get_data_medians)")
  
  dat_03
  
}

if (FALSE){
  
  # debugonce(get_data_raw)
  get_data_raw("HG", "Gadus morhua", "Muskel", "WWa", 2021)
  
  check <- get_data_raw("HCB", "Gadus morhua", "Lever", "WW", 2021)
  ggplot(check, aes(MYEAR, Value, color = is.na(FLAG1))) +
    geom_point() +
    scale_y_log10() +
    facet_wrap(vars(STATION_CODE))
  
  check <- get_data_raw("HCB", "Mytilus edulis", "Whole soft body", "WW", 2021)
  ggplot(check, aes(MYEAR, Value, shape = is.na(FLAG1))) +
    geom_point() +
    scale_y_log10() +
    scale_shape_manual(values = c(6, 19)) +
    facet_wrap(vars(STATION_CODE))
  check %>% filter(!is.na(FLAG1))
  
}



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# get_data_trends
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


get_data_trends <- function(data_medians,
                            filename_trends = "Data/125_results_2021_07_output/126_df_trend_2021.rds",
                            include_year){
  
  dat_trends_all <- readRDS(filename_trends) %>%
    filter(
      PARAM %in% unique(data_medians$PARAM),
      LATIN_NAME %in% unique(data_medians$LATIN_NAME),
      TISSUE_NAME %in% unique(data_medians$TISSUE_NAME),
      STATION_CODE %in% unique(data_medians$STATION_CODE),
      Basis %in% unique(data_medians$Basis)
      )
  
  check1 <- xtabs(~Trend_type, dat_trends_all)                 # should be equally many long and short 
  if (check1[1] != check1[2]){
    stop("There should be equally many rows for 'long' and 'short'")
  }
  
  #  xtabs(~Trend_string + Trend_type, dat_trends)
  
  # xtabs(~TISSUE_NAME + Trend_string, dat_trends)
  # xtabs(~STATION_CODE + TISSUE_NAME, dat_trends %>% filter(TISSUE_NAME %in% "Whole soft body"))
  
  dat_trends_01 <- data_medians %>%
    filter(MYEAR %in% include_year) %>%
    distinct(PARAM, STATION_CODE, LATIN_NAME, TISSUE_NAME, Basis, Station) %>%
    left_join(dat_trends_all,
              by = c("PARAM", "STATION_CODE", "LATIN_NAME", "TISSUE_NAME", "Basis"))
  
  dat_trends_02 <-  dat_trends_01 %>%
    mutate(
      # Shape and colour: Drop "Too few over-LOQ years" and "Too few years:
      Trend_shape = case_when(
        Trend_string %in% c("Increasing" , "Decreasing", "No change") ~ Trend_string, 
        Trend_string %in% "No trend" ~ "No change", 
        TRUE ~ as.character(NA)),
      Trend_shape = factor(Trend_shape, levels = shape_order),
      Trend_color = Trend_shape,
      # Text: Keep only "Too few over-LOQ years", "Too few years" and "No data before 2012":
      Trend_text = case_when(
        Trend_string %in% c("Increasing" , "Decreasing", "No change", "No trend") ~ as.character(NA), 
        Trend_string %in% "Estimation failed" ~ "No trend",
        TRUE ~ Trend_string),
      Trend_shape = factor(Trend_shape, levels = shape_order)
    )
  
  dat_trends_02
  
}
  
if (FALSE){

  # debugonce(get_data_trends)
  get_data_trends(dat_medians_list[[1]], include_year = current_year)
  

}

