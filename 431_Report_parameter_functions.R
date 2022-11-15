


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# get_data_medians
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


get_data_medians <- function(param, species, tissue, basis, include_year,
                             folder_110 = "Data",
                             filename_110 = "110_mediandata_updated_2022-09-23.rds",
                             filename_lookup_substancegroups = "Input_data/Lookup_tables/Lookup table - substance groups.csv",
                             filename_lookup_stations= "Input_data/Lookup_tables/Lookup_stationorder.csv",
                             filename_lookup_eqs = "Input_data/Lookup_tables/Lookup_EQS_limits.csv",
                             filename_lookup_proref = "Input_data/Lookup_tables/Lookup_proref.csv",
                             if_not_latest = "error"){
  
  indexvars <- c("PARAM", "STATION_CODE", "TISSUE_NAME", "LATIN_NAME", "Basis")
  
  check_latest(filename_110, folder_110, "110_mediandata_updated_", reaction = if_not_latest)
  
  filename_110_full <- paste0(folder_110, "/", filename_110)
  
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
  dat_medians_01 <- readRDS(filename_110_full) %>%
    dplyr::filter(PARAM %in% param,
                  LATIN_NAME %in% species, 
                  TISSUE_NAME %in% tissue,
                  Basis %in% basis)
  } else {
    # if Basis = WWa, we use WWa when available, otherwise we use WW
    dat_medians_wide <- readRDS(filename_110_full) %>%
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
    filter(Series_last_year >= include_year)
  
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
  
  # Set Proref_ratio and EQS_ratio
  # - but not if there is a under-LOQ value  and it is above the threshold 
  dat_medians_04 <- dat_medians_04 %>%
    mutate(
      Proref_ratio = ifelse(Proref < Value & Over_LOQ/N_median < 0.5, 
                            as.numeric(NA),
                            as.numeric(Value/Proref)), 
      EQS_ratio = ifelse(EQS < Value & Over_LOQ/N_median < 0.5, 
                         as.numeric(NA),
                         as.numeric(Value/EQS))
    )
  
  # Make sure these are properly numeric    
  dat_medians_04$Proref_ratio <- as.numeric(dat_medians_04$Proref_ratio)
  dat_medians_04$EQS_ratio <- as.numeric(dat_medians_04$EQS_ratio)
  
  dat_medians_04 <- dat_medians_04 %>%
    mutate(
      Above_EQS = case_when(
        EQS_ratio > 1 ~ "Over",
        EQS_ratio <= 1 ~ "Under",
        TRUE ~ as.character(NA)),
      Prop_underLOQ = 1 - Over_LOQ/N_median,
      FLAG1 = case_when(
        Prop_underLOQ < 0.5 ~ as.character(NA),
        Prop_underLOQ >= 0.5 ~ "<"),
      LOQ_label = ifelse(Prop_underLOQ >= 0.5, "<", "")
    ) %>%
    ungroup() 
  
  # Add 'Proref_ratio_cut'  
  # proref_max <- max(dat_medians_04$Proref_ratio, na.rm = TRUE)
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
  
  check <- get_data_medians("HG", "Gadus morhua", "Muskel", "WWa", 2021)

  debugonce(get_data_medians)
  check <- get_data_medians("DDEPP", "Mytilus edulis", "Whole soft body", "WW", 2020)
  
}


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# get_data_loq
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

get_loq <- function(param, basis = "WW"){
  
  dat_raw <- readRDS("Data/109_adjusted_data_2022-09-01.rds")  
  
  if (basis != "WW"){
    stop("Not yet implemented for basis other than WW")
  }
  
  dat_raw_sel <- dat_raw %>%
    filter(PARAM %in% param & c(TISSUE_NAME %in% c("Whole soft body", "Lever"))) %>%
    group_by(PARAM, MYEAR)
    
  result1 <- dat_raw_sel %>%
    summarise(
      Prop_underLOQ = mean(!is.na(FLAG1)),
      .groups = "drop"
    )
  
  result2 <- dat_raw_sel %>%
    filter(!is.na(FLAG1)) %>%
    summarise(
      min_LOQ = min(VALUE_WW, na.rm = TRUE),
      med_LOQ = median(VALUE_WW, na.rm = TRUE),
      max_LOQ = max(VALUE_WW, na.rm = TRUE),
      .groups = "drop"
    )
  
  result <- result1 %>%
    left_join(result2, by = c("MYEAR", "PARAM"))
  
  # Get rid of NA values (column 'med_LOQ' only)
  
  loq <- result[["med_LOQ"]]
  n <- length(loq)
  
  if (sum(!is.na(loq)) > 0){
    
    # If the first value is NA, use the first non-NA value
    if (is.na(loq[1]))
      loq[1] <- head(loq[!is.na(loq)],1)
    # If the last value is NA, use the last non-NA value
    if (is.na(loq[n]))
      loq[n] <- tail(loq[!is.na(loq)],1)
    
    loq <- zoo::na.approx(loq)
    
    result[["med_LOQ"]] <- loq
    
  }
  
  result
  
}

if (FALSE){
  
  debugonce(get_loq)
  get_loq("AG")
  get_loq("CB_S7_exloq")
   
}


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# get_data_raw
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


get_data_raw <- function(param, species, tissue, basis, include_year,
                         filename_109 = "Data/109_adjusted_data_2022-09-23.rds",
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
                            basis,
                            include_year){
  
  spp <- unique(data_medians$LATIN_NAME)

  dat_trends_list <- list()
  for (species in spp){
    
    if (basis == "WWa" & species != "Gadus morhua"){
      basis <- "WW"
    }
    
    dat_trends_list[[species]] <- readRDS(filename_trends) %>%
      filter(
        PARAM %in% unique(data_medians$PARAM),
        LATIN_NAME %in% species,
        TISSUE_NAME %in% unique(data_medians$TISSUE_NAME),
        STATION_CODE %in% unique(data_medians$STATION_CODE),
        Basis %in% basis
      )
  }
  
  dat_trends_all <- bind_rows(dat_trends_list)
  
  if (nrow(dat_trends_all) > 0){
  
  check1 <- xtabs(~Trend_type, dat_trends_all)                 # should be equally many long and short 
  if (check1[1] != check1[2]){
    stop("There should be equally many rows for 'long' and 'short'")
  }
  
  #  xtabs(~Trend_string + Trend_type, dat_trends)
  
  # xtabs(~TISSUE_NAME + Trend_string, dat_trends)
  # xtabs(~STATION_CODE + TISSUE_NAME, dat_trends %>% filter(TISSUE_NAME %in% "Whole soft body"))
  
  data_medians_series <- data_medians %>%
    filter(MYEAR %in% include_year) %>%
    distinct(PARAM, STATION_CODE, LATIN_NAME, TISSUE_NAME, Station)
  
  # This is done to ensure that stations lacking in 'dat_trends_all' are included as two rows
  #   although without trend info
  data_medians_series <- bind_rows(
    data_medians_series %>% mutate(Trend_type = "long"),
    data_medians_series %>% mutate(Trend_type = "short")
  )
  
  dat_trends_01 <- data_medians_series %>%
    left_join(dat_trends_all,
              by = c("PARAM", "STATION_CODE", "LATIN_NAME", "TISSUE_NAME", "Trend_type"))
  
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
        Trend_string %in% "Estimation failed" ~ "No model",
        is.na(Trend_string) ~ "Too few over-LOQ years",
        TRUE ~ Trend_string),
      Trend_shape = factor(Trend_shape, levels = shape_order)
    )
  
  result <- dat_trends_02
  
  } else {
    
    result <- NULL
    
  }
  
  result
  
}
  
if (FALSE){

  # debugonce(get_data_trends)
  get_data_trends(dat_medians_list[[1]], include_year = current_year)
  

}



get_data_trends2 <- function(data_medians,
                             filename_trends = "Data/125_results_2021_07_output/126_df_trend_2021.rds",
                             basis,
                             include_year){
  
  spp <- unique(data_medians$LATIN_NAME)
  
  dat_trends_list <- list()
  for (species in spp){
    
    if (basis == "WWa" & species != "Gadus morhua"){
      basis <- "WW"
    }
    
    dat_trends_list[[species]] <- readRDS(filename_trends) %>%
      filter(
        PARAM %in% unique(data_medians$PARAM),
        LATIN_NAME %in% species,
        TISSUE_NAME %in% unique(data_medians$TISSUE_NAME),
        STATION_CODE %in% unique(data_medians$STATION_CODE),
        Basis %in% basis
      )
  }
  
  dat_trends_all <- bind_rows(dat_trends_list)
  dat_trends_all
}

# get_data_trends2(dat_medians_list[[1]], basis = "WW", include_year = 2021) 


get_caption_text <- function(reference_values, type1, type2, basis1, basis2, unit){
  value1_txt <- paste(unique(reference_values$Ref_value1), collapse = ", ")
  caption_text1 <- glue("All data are given in {unit} w.w.")
  if (length(unique(reference_values$Ref_value1)) > 1){
    caption_text2 <- with(reference_values, glue("{type1}: {Ref_value1} {unit} {basis1} ({LATIN_NAME})"))
  } else {
    caption_text2 <- glue("{type1}: {unique(reference_values$Ref_value1)} {unit} {basis1} (all species)")
  }
  caption_text <- paste(caption_text1, "<br>", paste(caption_text2, collapse = "<br> "))  
  if (!is.null(reference_values$Ref_value2)){
    if (length(unique(reference_values$Ref_value2)) > 1){
      caption_text3 <- with(reference_values, glue("{type2}: {Ref_value2} {unit} {basis2} ({LATIN_NAME})"))
    } else {
      caption_text3 <- glue("{type2}: {unique(reference_values$Ref_value2)} {unit} {basis2} (all species)")
    }
    caption_text <- paste(caption_text, "<br>", paste(caption_text3, collapse = "<br> "))  
  }

  caption_text
  
}

if (FALSE){
  
  # test
  lookup_ref_value <- tribble(
    ~LATIN_NAME, ~Ref_value1, ~Ref_value2, ~Perc_dry_weight,  
    "Nucella lapillus", 5, 12, 32.8,
    "Littorina littorea", 5, 12, 21.9, 
  )
  get_caption_text(lookup_ref_value, "BAC", "EAC", "dw", "dw", "µg/kg")
  
  lookup_ref_value <- tribble(
    ~LATIN_NAME, ~Ref_value1, ~Ref_value2, ~Perc_dry_weight,  
    "Nucella lapillus", 5, 12, 32.8,
    "Littorina littorea", 8, 20, 21.9, 
  )
  debugonce(get_caption_text)
  get_caption_text(lookup_ref_value, "BAC", "EAC", "dw", "dw", "µg/kg")
}

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# APPENDIX: LOQ ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

if (FALSE){
  
  dat_raw <- readRDS("Data/109_adjusted_data_2022-09-23.rds")  
  
  dat_medians <- readRDS("Data/110_mediandata_updated_2022-09-23.rds")
  
  dat_trends <- readRDS("Data/125_results_2021_07_output/126_df_trend_2021.rds")  
  dat_trends %>%
    dplyr::filter(PARAM %in% "TBT" & Basis == "WW") %>% View("tbt")
   
  
  # LOQ median values for all parameters
  dat_raw %>%
    filter(PARAM %in% c("BDE47", "AG", "ANT", "BAA", "BAP", "BDE100", "BDE209", 
                        "CB118", "CB138", "CB153", "CD", "CO", "DDEPP", "FLU", "HBCDA", 
                        "HCB", "HG", "NAP", "NI", "PB", "PFOA", "PFOS", "PFOSA", "BDE99", "ZN") & 
             TISSUE_NAME %in% c("Whole soft body", "Lever")) %>%
    group_by(PARAM, MYEAR) %>%
    summarise(
      medLOQ = median(VALUE_WW[!is.na(FLAG1)], na.rm = TRUE)
    ) %>% 
    tidyr::pivot_wider(names_from = PARAM, values_from = medLOQ) %>%
    arrange(MYEAR) %>%
    tail(15)
  
  # Proportion of values below parameters
  check <- dat_raw %>%
    filter(PARAM %in% c("BDE47", "AG", "ANT", "BAA", "BAP", "BDE100", "BDE209", 
                        "CB118", "CB138", "CB153", "CD", "CO", "DDEPP", "FLU", "HBCDA", 
                        "HCB", "HG", "NAP", "NI", "PB", "PFOA", "PFOS", "PFOSA", "BDE99", "ZN") & 
             TISSUE_NAME %in% c("Whole soft body", "Lever")) %>%
    mutate(LATIN_NAME = substr(LATIN_NAME, 1, 3)) %>%
    group_by(PARAM, MYEAR, LATIN_NAME) %>%
    summarise(
      UnderLOQ = mean(!is.na(FLAG1))
    ) %>% 
    tidyr::pivot_wider(names_from = c(PARAM, LATIN_NAME), values_from = UnderLOQ) %>%
    arrange(MYEAR)
  tail(check, 15)
  # View(check)

  # LOQ median values for PCB7
  dat_raw %>%
    filter(PARAM %in% c("CB28", "CB52", "CB101", "CB118", "CB138", "CB153", "CB180")  & 
             TISSUE_NAME %in% c("Whole soft body", "Lever")) %>%
    group_by(PARAM, MYEAR) %>%
    summarise(
      medLOQ = median(VALUE_WW[!is.na(FLAG1)], na.rm = TRUE),
    ) %>% 
    tidyr::pivot_wider(names_from = PARAM, values_from = medLOQ) %>%
    arrange(MYEAR) %>%
    tail(15)
  
  # Proportion of values below LOQ, PCB7 
  dat_raw %>%
    filter(PARAM %in% c("CB28", "CB52", "CB101", "CB118", "CB138", "CB153", "CB180")  & 
             TISSUE_NAME %in% c("Whole soft body", "Lever")) %>%
    mutate(LATIN_NAME = substr(LATIN_NAME, 1, 3)) %>%
    group_by(PARAM, MYEAR, LATIN_NAME) %>%
    summarise(
      UnderLOQ = mean(!is.na(FLAG1))
    ) %>% 
    tidyr::pivot_wider(names_from = c(PARAM, LATIN_NAME), values_from = UnderLOQ) %>%
    arrange(MYEAR) %>%
    tail(15)
  
  

  # LOQ values for one parameter
  dat_raw %>%
    filter(PARAM %in% "CO" & c(TISSUE_NAME %in% c("Whole soft body", "Lever") & MYEAR >= 2008)) %>%
    group_by(PARAM, MYEAR) %>%
    summarise(
      Prop_underLOQ = mean(!is.na(FLAG1)),
      min_value = min(VALUE_WW[is.na(FLAG1)], na.rm = TRUE),
      min_LOQ = min(VALUE_WW[!is.na(FLAG1)], na.rm = TRUE),
      med_LOQ = median(VALUE_WW[!is.na(FLAG1)], na.rm = TRUE),
      max_LOQ = max(VALUE_WW[!is.na(FLAG1)], na.rm = TRUE)
    ) %>% tail(20)
  
  
}
