

#
# Select rows from Milkys data ----
#

# get_data_for_trend
# Flexible way of selecting rows of data for trend analysis
milkys_select_rows <- function(param = NULL, species = NULL, tissue = NULL, station = NULL, firstyear = NULL, data){
  data_sel <- data
  if (!is.null(param))
    data_sel <- data_sel %>% dplyr::filter(PARAM %in% param)
  if (!is.null(species))
    data_sel <- data_sel %>% dplyr::filter(LATIN_NAME %in% species)
  if (!is.null(tissue))
    data_sel <- data_sel %>% dplyr::filter(TISSUE_NAME %in% tissue)
  if (!is.null(station)){
    # For station, we need only to give the first letters, e.g. "30B" for "30B Inner Oslofjord"
    # data_sel <- data_sel %>%
    #   mutate(Station_short = substr(Station, 1, nchar(station))) %>%
    #   dplyr::filter(Station_short %in% station)
    # For station, we need only to give  partil match, e.g. "Inner Osl" for "30B Inner Oslofjord"
    data_sel <- data_sel %>%
      dplyr::filter(grepl(station, Station, fixed = TRUE))
  }
  if (!is.null(firstyear))
    data_sel <- data_sel %>% dplyr::filter(MYEAR >= firstyear)
  data_sel
}

#
# Retrieve tissue, trend, etc. ----
#

get_tissue <- function(param, species){
  
  fish_species <- c("Gadus morhua", "Limanda limanda", 
                    "Somateria mollissima", "Platichthys flesus", "Modiolus modiolus")
  mollusc_species <- c("Mytilus edulis", "Nucella lapillus", "Littorina littorea")
  bird_species <- c("Somateria mollissima")
  bile_parameters <- c("AY", "PYR1O", "BILIV", "OHBIL", "PA1O", "BAP3O", "ABS380", 
                       "BAP3OH", "PA1OH", "PYR1OH", "PYR1", "AY380", "1-OH-fenantren")
  livermicrosome_parameters <- c("PROTV", "AY380")
  
  if (!species %in% c(fish_species, mollusc_species, bird_species)){
    stop("Species not found in the code of function 'get_tissue'. Check this function.")
  } 
  
  if (species %in% bird_species){
    stop("BIRD DATA - PICK TISSUE MANUALLY")
  } 
  
  if (param %in% "HG"){
    if (species %in% fish_species){
      result <- "Muskel"
    } else if (species %in% mollusc_species){
      result <- "Whole soft body"
    }
  } else if (param %in% bile_parameters){
    if (species %in% fish_species){
      result <- "Galle"
    } else {
      stop("Bile parameter measured in bird or mollusc? Check data.")
    }
  } else if (param %in% livermicrosome_parameters){
    if (species %in% fish_species){
      result <- "Liver - microsome"
    } else {
      stop("Liver microsome parameter measured in bird or mollusc? Check data.")
    }
  } else {
    if (species %in% fish_species){
      result <- "Lever"
    } else if (species %in% mollusc_species){
      result <- "Whole soft body"
    }
  }
  result
}

if (FALSE){
  get_tissue("HG", "Gadus morhua")
  get_tissue("CB118", "Gadus morhua")
  get_tissue("HG", "Mytilus edulis")
  get_tissue("CB118", "Mytilus edulis")
  get_tissue("PYR1O", "Gadus morhua")
  get_tissue("PYR1O", "Mytilus edulis")
}

# Get ordinary linear trend
# Selects rows of data, then performs trend analysis
get_trend <- function(param, species, tissue, station, firstyear = NULL, data,
                      y = "VALUE_WW_med"){
  data_sel <- milkys_select_rows(param=param, species=species, tissue=tissue, 
                                 station=station, firstyear = firstyear, data=data)
  sel_var <- names(data_sel) %in% y
  if (sum(sel_var) == 0)
    stop("Variable ", sQuote(y), "not found in dataset!")
  if (sum(sel_var) >= 2)
    stop("Several variables named ", sQuote(y), "found in dataset!")
  names(data_sel)[sel_var] <- "y"
  if (nrow(data_sel) >= 3){
    mod <- lm(log(y)  ~ MYEAR, data = data_sel)
    coef <- data.frame(summary(mod)$coef)
    result <- coef[2,]
    names(result) <- c("Est", "SE", "t", "P")
    result_meta <- data.frame(
      PARAM = param, LATIN_NAME = species, 
      TISSUE_NAME = tissue, Station = station)
    result <- cbind(result_meta, result)
  } else {
    result <- NULL
  }
  result
}

#
# get_trend for leftcensored data for one data set, assumed to be a single time series  
#
# Used by 'get_trendobj_station'
#
# The assumption that it's a single time series is checked   
# Selects rows of data, then performs trend analysis
#
get_trend_cens <- function(selected_data, firstyear = NULL,
                           several_series_is_error = TRUE,
                           ...){
  
  check <- unique(selected_data$PARAM)
  if (length(check) > 1 & several_series_is_error){
    stop("Series contains several parameters: ", paste(check, collapse = ", "))
  }
  check <- unique(selected_data$LATIN_NAME)
  if (length(check) > 1 & several_series_is_error){
    stop("Series contains several species: ", paste(check, collapse = ", "))
  }
  check <- unique(selected_data$TISSUE_NAME)
  if (length(check) > 1 & several_series_is_error){
    stop("Series contains several tissues: ", paste(check, collapse = ", "))
  }
  
  check <- unique(selected_data$Station)
  if (length(check) > 1 & several_series_is_error){
    stop("Series contains several stations: ", paste(check, collapse = ", "))
  }
  
  data_analysis <- leftcensored_prepare(
    data = selected_data, 
    var_year = "MYEAR", var_concentration = "VALUE_WW_med", var_LOQflag = "FLAG1")
  # Linear MCMC  
  result_full <- lc_linear(data_analysis, ...)
  result_full
}

#
# get_trend for leftcensored data, one time series  
#
# Selects only station - meant to be used for data with only one parameter (and species/tissue)
# Used by 'get_trendobj_parameter_species'  
#
# keep = the parts of the object we want to keep (to decrease memory usage)
# - set keep = "all" to keep all parts   

get_trendobj_station <- function(station, data, 
                                 keep = c("intercept", "slope", "plot_data"),
                                 ...){   
  data_select <- milkys_select_rows(station = station, data = data) 
  
  trendobj_complete <- get_trend_cens(data_select, ...) 
  # data_select
  
  if (keep == "all"){
    trendobj_complete
  } else {
    trendobj_complete[keep]
  }
}

# test
# trendobj1 <- get_trendobj_station("56A", dat_sel_medium)
# trendobj2 <- get_trendobj_station("56A", dat_sel_medium, firstyear = 2011)
#
# Should make error:
# test <- get_trendobj_station("56A", subset(dat_medium, PARAM %in% c("CD", "PB")))



#
# get_trend for leftcensored data, all time series for one parameter and species  
#
# Uses 'get_trend_cens'

get_trendobj_parameter_species <- function(param, species, data){
  
  data_sel <- data %>% dplyr::filter(PARAM %in% param)
  
  # Vector of stations
  stations <- unique(data_sel$Station) %>% 
    set_names() # set names of 'stations' to stations (will be carried over to 'trend_results')
  
  # Filename for saving
  fn <- paste0("Trends ", unique(data_sel$PARAM), " ", species, ".rds")
  fn_full <- paste0("Data/824_trend_results/", fn)
  
  file_exists <- fn %in% dir("Data/824_trend_results")
  
  # If file of results exists, reada it; otherwise, perform anlysis and write it  
  if (file_exists){
    message("Trends have already been calculated and are read from ", sQuote(fn_full))
    trend_results <- readRDS(fn_full)
  } else {
    
    # Run all (takes some minutes)
    trend_results <- list()
    trend_results[[1]] <- purrr::map(stations, get_trendobj_station_s, data = data_sel)
    trend_results[[2]] <- purrr::map(stations, get_trendobj_station_s, data = data_sel, firstyear = 2011)
    
    saveRDS(trend_results, fn_full)
    
  }
  
  invisible(trend_results)
  
}


#
# Utility functions ----
#

#
# Create function which creates relative class, based on two thresholds  
# - by default, works on log scale  
# - 'lowest' (= relclass 1.0)  could be taken to be just below the lowest value observed 
#
create_class_function <- function(thresh1, thresh2,  
                                  lowest = 0, log = TRUE,
                                  from_class = 1, to_class = 100){
  
  # Make class data
  df_classes_all <- data.frame(
    conc = c(lowest, thresh1, thresh2),
    class = 1:3
  )
  
  if (to_class > 3){
    df_classes_all <- bind_rows(
      df_classes_all,
      data.frame(
        conc = seq(2,(to_class-2))*thresh2,
        class = seq(4,to_class))
    )
  }
  
  df_classes <- df_classes_all %>%
    dplyr::filter(class >= from_class & class <= to_class)
  
  if (log){
    fun <- approxfun(log(df_classes$conc), df_classes$class)
    result <- function(x){ fun(log(x))  }
  } else {
    result <- approxfun(df_classes$conc, df_classes$class)
  }
  
  result
  
}

# TEST
if (FALSE){
  f1 <- create_class_function(thresh1 = 20, thresh2 = 500, log = FALSE,
                              from_class = 1, to_class = 100)
  f1(c(8, 20, 30, 300, 500, 700))
  
  f2 <- create_class_function(thresh1 = 20, thresh2 = 500, log = TRUE,
                              from_class = 1, to_class = 100)
  f2(c(8, 20, 30, 300, 500, 700))
}

#
# Data on sample level ----  
#

get_parametervalues_singlegroup <- function(paramgroup, return_group_names = FALSE){
  
  #
  # Parameter groups
  #
  lookup_paramgroup <- readxl::read_excel("Input_data/Lookup table - substance groups.xlsx")
  group_names <- unique(lookup_paramgroup$Substance.Group)
  
  if (return_group_names){
    return <- unique(lookup_paramgroup$Substance.Group)
  } else {
    sel_paramgroup <- grep(paramgroup, group_names, value = TRUE, ignore.case = TRUE)
    if (length(sel_paramgroup) > 1){
      stop(paramgroup, " fits several names: ", paste(sel_paramgroup, collapse = "; "))
    }
    lookup_paramgroup <- lookup_paramgroup %>%
      dplyr::filter(Substance.Group %in% sel_paramgroup)
    
    # Parameters
    return <- unique(lookup_paramgroup$PARAM)
  }
  
  return  
  
}

if (FALSE){
  get_parametervalues_singlegroup(return_group_names = TRUE)
  get_parametervalues_singlegroup("metals")
  get_parametervalues_singlegroup("organobrom")
  get_parametervalues_singlegroup("chlor")      # should return error
  get_parametervalues_singlegroup("chlorobi")
  get_parametervalues_singlegroup("biological")
  get_parametervalues_singlegroup("biomarkers")
  get_parametervalues_singlegroup("paraffins")
  get_parametervalues_singlegroup("DDTs")
  get_parametervalues_singlegroup("cyclohexanes")
  get_parametervalues_singlegroup("organochlorine")
}

# Get parameter names for several groups
# If no argument given, returns group names
get_parametervalues <- function(paramgroups = NULL){
  
  if (is.null(paramgroups)){
    result2 <- get_parametervalues_singlegroup(return_group_names = TRUE)
  } else {
    result1 <- lapply(paramgroups, get_parametervalues_singlegroup)
    
    for (i in 1:length(result1)){
      if (i == 1){
        result2 <- result1[[1]]
      } else {
        result2 <- c(result2, result1[[i]])
      }
    }
  }
  result2
}

if (FALSE){
  get_parametervalues("organochlorine")
  get_parametervalues(c("organochlorine", "cyclohexanes"))
  get_parametervalues()
}

get_data_tables <- function(paramgroup,
                            param = NULL,
                            filename_109 = "Data/109_adjusted_data_2022-09-01.rds",
                            filename_110 = "Data/110_mediandata_updated_2022-09-01.rds",
                            filename_lookup_substancegroups = "Input_data/Lookup table - substance groups.xlsx",
                            filename_lookup_stations= "Input_data/Lookup_tables/Lookup_stationorder.csv",
                            filename_lookup_eqs = "Input_data/EQS_limits.csv",
                            filename_lookup_proref = "Input_data/Lookup_tables/Lookup_proref.csv"){
  
  
  # Parameter names
  if (is.null(param)){
    param_values <- get_parametervalues(paramgroup)
  } else {
    param_values <- param
  }
    
  # Parameter groups
  lookup_paramgroup <- read_excel(filename_lookup_substancegroups) %>%
    dplyr::filter(PARAM %in% param_values)

  # Raw data
  dat_all <- readRDS(filename_109) %>%
    dplyr::filter(PARAM %in% param_values)
  
  # Medians
  dat_medians <- readRDS(filename_110) %>%
    dplyr::filter(PARAM %in% param_values) %>%
    rename(Proref = Q95)
  
  # Stations
  lookup_stations <- read.csv(filename_lookup_stations)
  
  # EQS and proref   
  lookup_proref <- read.csv(filename_lookup_proref) %>%
    filter(PARAM %in% param) 
  
  lookup_eqs <- read.csv(filename_lookup_eqs) %>%
    filter(PARAM %in% param) %>%
    rename(EQS = Limit) %>%
    select(-Long_name, Kommentar)

  check <- lookup_proref %>%
    add_count(PARAM, LATIN_NAME, TISSUE_NAME, Basis) %>%
    dplyr::filter(n > 1)
  
  if (nrow(check) > 0){
    stop("More than one EQS per parameter/species/tissue/basis!")
  }
  
  list(data = dat_all, 
       medians = dat_medians,
       lookup_paramgroup = lookup_paramgroup, 
       lookup_stations = lookup_stations, 
       lookup_proref = lookup_proref,
       lookup_eqs = lookup_eqs
       )
  
}

# Test
if (FALSE){
  X <- get_data_tables("metals")
}


get_data <- function(paramgroup, param = NULL, speciesgroup, min_obs = 100){
  
  # Parameter names
  if (is.null(param)){
    X <- get_data_tables(paramgroup)
  } else {
    X <- get_data_tables(param = param)
  }
  
  
  
  X$lookup_stations <- X$lookup_stations %>%
    arrange(Station_order) %>%
    mutate(
      Station = paste(STATION_CODE, Station_short),        # This will be shown in graphs - make changes here
      Station2 = substr(Station, 1, 15),                   # This will be shown in graphs - make changes here
      Station_name = forcats::fct_inorder(Station_name),
      Station = forcats::fct_inorder(Station),
      Station2 = forcats::fct_inorder(Station2),
      Water_region = forcats::fct_inorder(Water_region)
    )
  
  dat_1 <- X$dat_all %>%
    left_join(X$lookup_paramgroup %>% select(PARAM, Substance.Group), 
              by = "PARAM") %>%
    add_count(PARAM) %>%
    dplyr::filter(n >= min_obs) %>%
    # Add 'Station.Name'
    left_join(X$lookup_stations, by = "STATION_CODE")
  
  dat_2 <- dat_1 %>%
    left_join(X$lookup_eqs_ww, by = c("PARAM", "LATIN_NAME", "TISSUE_NAME")) %>%
    mutate(
      Above_EQS = case_when(
        VALUE_WW > EQS_WW ~ "Over",
        VALUE_WW <= EQS_WW ~ "Under",
        TRUE ~ as.character(NA))
    ) 
  
  if (speciesgroup == "fish"){
    
    result <- dat_2 %>%
      dplyr::filter(LATIN_NAME %in% c("Gadus morhua", "Platichthys flesus"))
    
  } else if (grepl("mussel", speciesgroup)){
    
    result <- dat_2 %>%
      dplyr::filter(LATIN_NAME %in% c("Mytilus edulis"))
    
  } else {
    
    result <- dat_2
  }
  
  for (col in c("Station_name", "Station", "Station2", "Water_region"))
    result[[col]] <- droplevels(result[[col]])
  
  result
  
}

# Test
if (FALSE){
  # debugonce(get_data)
  x1 <- get_data("metals", "fish")
  x2 <- get_data("metals", "mussel")
}



get_data_param <- function(param, speciesgroup, min_obs = 100){
  
  X <- get_data_tables(paramgroup)
  
  X$lookup_stations <- X$lookup_stations %>%
    arrange(Station_order) %>%
    mutate(
      Station = paste(STATION_CODE, Station_short),        # This will be shown in graphs - make changes here
      Station2 = substr(Station, 1, 15),                   # This will be shown in graphs - make changes here
      Station_name = forcats::fct_inorder(Station_name),
      Station = forcats::fct_inorder(Station),
      Station2 = forcats::fct_inorder(Station2),
      Water_region = forcats::fct_inorder(Water_region)
    )
  
  dat_1 <- X$dat_all %>%
    left_join(X$lookup_paramgroup %>% select(PARAM, Substance.Group), 
              by = "PARAM") %>%
    add_count(PARAM) %>%
    dplyr::filter(n >= min_obs) %>%
    # Add 'Station.Name'
    left_join(X$lookup_stations, by = "STATION_CODE")
  
  dat_2 <- dat_1 %>%
    left_join(X$lookup_eqs_ww, by = c("PARAM", "LATIN_NAME", "TISSUE_NAME")) %>%
    mutate(
      Above_EQS = case_when(
        VALUE_WW > EQS_WW ~ "Over",
        VALUE_WW <= EQS_WW ~ "Under",
        TRUE ~ as.character(NA))
    ) 
  
  if (speciesgroup == "fish"){
    
    result <- dat_2 %>%
      dplyr::filter(LATIN_NAME %in% c("Gadus morhua", "Platichthys flesus"))
    
  } else if (grepl("mussel", speciesgroup)){
    
    result <- dat_2 %>%
      dplyr::filter(LATIN_NAME %in% c("Mytilus edulis"))
    
  } else {
    
    result <- dat_2
  }
  
  for (col in c("Station_name", "Station", "Station2", "Water_region"))
    result[[col]] <- droplevels(result[[col]])
  
  result
  
}

# Test
if (FALSE){
  # debugonce(get_data)
  x1 <- get_data("metals", "fish")
  x2 <- get_data("metals", "mussel")
}


#
# Data, medians per station/year
#


# * Including 'dat_medium_fish' and 'dat_medium_mussel' which will be used for "all-parameters + all-stations + last year" overviews  

get_medians <- function(data_samplelevel_fish, data_samplelevel_mussel){
  
  result <- bind_rows(data_samplelevel_fish, data_samplelevel_mussel) %>%
    group_by(Station) %>%
    mutate(n_after_2018 = sum(MYEAR >= 2018)) %>%
    dplyr::filter(
      n_after_2018 > 0) %>%
    ungroup() %>%
    group_by(PARAM, LATIN_NAME, TISSUE_NAME, STATION_CODE, Station, Station2, Water_region, MYEAR, UNIT, EQS_WW, Proref) %>%
    summarize(
      VALUE_WW_med = median(VALUE_WW, na.rm = TRUE),
      VALUE_WW_min = min(VALUE_WW, na.rm = TRUE),
      VALUE_WW_max = max(VALUE_WW, na.rm = TRUE),
      N = n(),
      N_underLOQ = sum(FLAG1 %in% "<"),
      .groups = "drop") %>%
    mutate(
      Proref_ratio_WW = VALUE_WW_med/Proref,
      EQS_ratio_WW = VALUE_WW_med/EQS_WW,
      Prop_underLOQ = N_underLOQ/N,
      `Detected %` = 100*(1-Prop_underLOQ),
      FLAG1 = case_when(
        Prop_underLOQ < 0.5 ~ as.character(NA),
        Prop_underLOQ >= 0.5 ~ "<"),
      Above_EQS = case_when(
        EQS_ratio_WW > 1 ~ "Over",
        EQS_ratio_WW <= 1 ~ "Under",
        TRUE ~ as.character(NA)),
      LOQ_label = ifelse(Prop_underLOQ >= 0.5, "<", "")
    )
  
  check <- xtabs(~MYEAR, result %>% dplyr::filter(PARAM == "HG" & grepl("30A", Station)))
  if (sum(check > 1) > 0){
    stop("Error in median: more than one measurement per parameter/station/year")
  }
  
  result
  
}

# Test
if (FALSE){
  debugonce(get_medians)
  x1 <- get_data("metals", "fish")
  x2 <- get_data("metals", "mussel")
  test <- get_medians(x1, x2)
}

#
# Results for parameter group, one year  ----
#

#
# Tile plot of all parameters in one year (for one parameter group)  
#

# This makes an htmlwidget with tooltip (if tooltip = TRUE, the default)
# or a ggplot

# Note: to be used for a single year.
# Can be extended to several years using code like this:
#
# group_by(Station2, PARAM) %>%
# summarize(
#   VALUE_WW_med = median(VALUE_WW_med, na.rm = TRUE),
#   LOQ_perc_under = mean(LOQ_label == "<", na.rm = TRUE)*100,
#   LOQ_label = case_when(
#     LOQ_perc_under == 0 ~ "",
#     LOQ_perc_under > 0 ~ paste(round(LOQ_perc_under,0), "% <LOQ")
#   )
# )

# NOTE: much of the code in the two functions 
# pargroup_median_table_tooltip and pargroup_median_table_static is
#   similar and could be put into a common "prepare data" function.
#   See code for 'parameter_median_table_tooltip' below
pargroup_median_table <- function(..., tooltip = TRUE){
  if (tooltip){
    pargroup_median_table_tooltip(...)
  } else {
    pargroup_median_table_static(...)
  }
}

pargroup_median_table_data <- function(data_medians, fill, year,
                                       breaks = "ratio"){
  
  if (length(year) > 1){
    stop("Several years given. Set year to be a single year")
  }
  
  data_medians$fill <- data_medians[[fill]]
  fill_column <- fill
  
  dat_plot <- data_medians %>%
    dplyr::filter(MYEAR %in% year) %>%
    arrange(desc(PARAM))
  
  # fill_min <- floor(1000*min(dat_plot$fill, na.rm = T))/1000
  # fill_max <- ceiling(max(dat_plot$fill, na.rm = T))
  fill_min <- 0.0001
  fill_max <- 1000
  
  if (is.numeric(breaks)){
    breaks_vector <- breaks
  } else if (breaks %in% "ratio"){
    breaks_vector = c(fill_min,0.5,0.75,0.9,1,2,3,5,10,fill_max)
  } else if (breaks %in% "0_to_100"){
    breaks_vector = seq(0, 100, 10)
  }
  
  dat_plot <- dat_plot %>% 
    mutate(
      PARAM = forcats::fct_inorder(PARAM),
      fill_cut = cut(fill, breaks = breaks_vector, include.lowest = TRUE),
      VALUE_WW_txt = paste0(
        fill_column, ": ", round(fill, 3), "<br>",
        "Median: ", LOQ_label, round(VALUE_WW_med, 4), " ug/kg<br>",
        "(", round(VALUE_WW_min, 4), "-", round(VALUE_WW_max, 4), "; N =", N, ")")) %>%
    select(Proref_ratio_WW, VALUE_WW_txt, MYEAR, Station2, PARAM, fill, fill_cut,
           Above_EQS, VALUE_WW_med, LOQ_label)
  
  dat_plot
  
  
}

if (F){
  # debugonce(pargroup_median_table_data)
  pargroup_median_table_data(dat_median_fish, fill = "Proref_ratio_WW", year = 2021)
}

pargroup_median_table_colors <- function(plotdata, breaks){
  
  n_levels <- length(levels(plotdata$fill_cut))
  
  if (is.numeric(breaks)){
    breaks_vector <- breaks
  } else if (breaks %in% "ratio"){
    cols <- c(RColorBrewer::brewer.pal(6, "Blues")[5:2],
              RColorBrewer::brewer.pal(6, "YlOrRd")[1:5])
  } else if (breaks %in% "0_to_100"){
    cols <- scico_to_hex("vik", 10)
  }
  
  
  # Note: aalternative color scales
  #scale_fill_viridis_b(trans = "log10", breaks = c(0.01,1,2,3,5,10,100), option = "plasma") +
  #scale_fill_binned(breaks = c(0.01,1,2,3,5,10,100)) +
  
  if (length(cols) != n_levels){
    cat("\nLevels of fill variable:", levels(plotdata$fill_cut), "\n")
    stop("Data has ", n_levels, " levels (see above) but ", length(cols), " colors given")
  }
  names(cols) <- levels(plotdata$fill_cut)
  cols
}

if (F){
  # debugonce(pargroup_median_table_tooltip)
  X <- pargroup_median_table_data(dat_median_fish, fill = "Proref_ratio_WW", year = 2021)
  pargroup_median_table_colors(X, breaks = "ratio")
  pargroup_median_table_colors(X, breaks = "0_to_100")
}

# Get hex values for scico colours
scico_to_hex <- function(palette, n, min = 1, max = 255){
  M <- scico::scico_palette_data(palette)[round(seq(1, 255, length = n), 0),]
  purrr::map_chr(1:10, ~rgb(M$r[.], M$g[.], M$b[.], maxColorValue=1, alpha=1))
}
if (FALSE){
  # Test
  x <- scico_to_hex("vik", 10)
  x
  pie(rep(1,10), col = x)
  # For scico colors and visualisation:
  scico::scico_palette_names()
  scico::scico_palette_show()
  scico::scico_palette_show("vik")
}

# "Dynamic" plot, i.e. with tooltips (returns htmlwidget / girafe object)  
pargroup_median_table_tooltip <- function(data_medians, fill, year,
                                          width_svg = 6, height_svg = 3.5, 
                                          breaks = "ratio",
                                          show_eqs = TRUE){
  
  fill_column <- fill
  
  dat_plot <- pargroup_median_table_data(
    data_medians= data_medians, fill = fill, year = year, breaks = breaks)
  
  cols <- pargroup_median_table_colors(dat_plot, breaks = breaks)
  
  p <- ggplot(dat_plot, aes(Station2, PARAM, tooltip = VALUE_WW_txt))
  
  if (show_eqs){
    # Shown as red tiles (which will be overplotted by slightly smaller tiles afterwards) 
    p <- p + 
      geom_tile(data = subset(dat_plot, Above_EQS %in% "Over"),
                color = "red", size = 1, height = 0.9, width = 0.9)
  }
  p <- p +
    geom_tile(aes(fill = fill_cut), width = 0.9, height = 0.9) +
    geom_text(aes(label = LOQ_label), size = 1.5, nudge_y = 0.2) +
    geom_text_interactive(aes(label = round(VALUE_WW_med, 3)), nudge_y = -0.1, size = 1.5) +
    scale_fill_manual(fill_column, values = cols) +
    scale_y_discrete(limits = levels(dat_plot$PARAM)) +
    theme_bw() +
    theme(
      axis.title = element_blank(),
      axis.text = element_text(size = 7),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8),
      axis.text.x = element_text(angle = -45, hjust = 0),
      panel.grid = element_blank()) 
  
  girafe(ggobj = p, height_svg = 3)
  
}

if (F){
  # debugonce(pargroup_median_table_tooltip)
  pargroup_median_table_tooltip(dat_median_fish, fill = "Proref_ratio_WW", year = 2021)
}


# Static plot (returns ordinary ggplot object)  
pargroup_median_table_static <- function(data_medians, fill, year, breaks = "ratio"){
  
  fill_column <- fill
  
  dat_plot <- pargroup_median_table_data(
    data_medians= data_medians, fill = fill, year = year, breaks = breaks)
  
  cols <- parameter_median_table_colors(dat_plot, breaks = breaks)
  
  gg <- ggplot(dat_plot, aes(Station2, PARAM, fill = fill)) +
    geom_tile()
  gg <- gg +
    geom_tile(data = subset(dat_plot, Above_EQS %in% "Over"),
              color = "red", size = 1, height = 0.9, width = 0.9) +
    geom_text(aes(label = round(VALUE_WW_med, 3)), nudge_y = -0.1, size = 3) +
    geom_text(aes(label = LOQ_label), size = 3, nudge_y = 0.3) +
    #scale_fill_viridis_b(trans = "log10", breaks = c(0.01,1,2,3,5,10,100), option = "plasma") +
    #scale_fill_binned(breaks = c(0.01,1,2,3,5,10,100)) +
    scale_fill_manual(fill, values = cols) +
    scale_color_manual(values = c("red", "white")) +
    scale_alpha_manual(values = c(1, 0)) +
    scale_y_discrete() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = -45, hjust = 0))
  labs(
    title = "Medians"
  )
  
  gg
  
}

if (F){
  # debugonce(pargroup_median_table_static)
  pargroup_median_table_static(dat_median_fish, fill = "Proref_ratio_WW", year = 2021)
}


pargroup_boxplot <- function(data_medians, y, year, ylabel = NULL, main_title = NULL){
  
  data_medians$y <- data_medians[[y]]
  
  if (is.null(ylabel))
    ylabel = y
  
  dat_prorefplot2 <- data_medians %>%    # Change here for fish vs. mussel
    dplyr::filter(MYEAR == year) %>%
    mutate(
      Unit = gsub("_P_", "/", UNIT, fixed = TRUE) %>% tolower(),
      Tooltip = paste0(Station, "<br>Conc.: (min-median-max): ", VALUE_WW_min, "-", VALUE_WW_med, "-", VALUE_WW_max, " ", Unit))
  
  # str(dat_prorefplot2)
  
  gg <- ggplot(dat_prorefplot2, aes(PARAM, y = y)) +
    geom_hline(yintercept = 1) +
    geom_boxplot() +
    geom_jitter_interactive(aes(fill = Water_region, tooltip = Tooltip, data_id = STATION_CODE), pch = 21, size = 2, width = 0.1) +
    # scale_fill_distiller("Along coast\n(far N/E = blue)", palette = "RdBu", direction = 1) +  # Geogr_position
    scale_fill_brewer("Water region", palette = "RdBu", direction = -1) +
    theme_bw() +
    ggeasy::easy_rotate_x_labels(angle = -45) +
    labs(y = ylabel, title = main_title)
  # gg
  
  # gg <- gg + coord_flip()
  # gg
  
  ggr <- girafe(ggobj = plot_grid(gg + guides(fill = "none") + labs(subtitle = "Medians, ordinary scale"),
                                  gg + scale_y_log10() + labs(subtitle = "Medians, log scale"), 
                                  rel_widths = c(1,1.35)), 
                width_svg = 10, height_svg = 4)
  
  ggr <- girafe_options(ggr, opts_hover(css = "fill:wheat;stroke:orange;r:5pt;") )
  
  ggr
  
}

if (FALSE){
  debugonce(pargroup_boxplot)
  
  pargroup_boxplot(dat_median_fish, y = "EQS_ratio_WW", year = 2021)
  
  pargroup_boxplot(dat_median_mussel, y = "Proref_ratio_WW", year = 2021)
  
  sel <- is.na(dat_list$lookup_eqs$LATIN_NAME)
  dat_list$lookup_eqs$LATIN_NAME[sel] <- species 
  dat_boxplot <- dat_list$data %>%
    left_join(dat_list$lookup_eqs, by = c("LATIN_NAME", "PARAM"))
  
  # debugonce(ratio_boxplot_interactive)
  

  
}


ratioplot <- function(data, x, y, 
                      fill = NULL, tooltip = NULL, data_id = NULL, 
                      ymin = NULL, ymax = NULL,
                      ylabel = NULL, main_title = NULL,
                      hline = 1,
                      hline_extra = NULL,
                      interactive){
  
  data$x <- data[[x]]
  data$y <- data[[y]]
  data$tooltip <- data[[tooltip]]
  data$fill <- data[[fill]]
  data$data_id <- data[[data_id]]

  if (!is.null(fill)){
    data$fill <- data[[fill]]
  } 
  
  if (!is.null(tooltip)){
    data$tooltip <- data[[tooltip]]
  } else {
    data$tooltip <- data[[y]]
  }
  
  if (!is.null(data_id)){
    data$tooltip <- data[[data_id]]
  } else {
    data$tooltip <- data[[x]]
  }
  
  if (!is.null(ymin))
    data$ymin <- data[[ymin]]
  if (!is.null(ymax))
    data$ymax <- data[[ymax]]
  
  
  if (is.null(ylabel))
    ylabel = y
  
  # str(dat_prorefplot2)
  
  gg <- ggplot(data, aes(x = x, y = y))
  
  if (!is.null(ymin) & !is.null(ymax))
    gg <- gg + geom_linerange(aes(ymin = ymin, ymax = ymax))
  
  if (is.numeric(hline))
    gg <- gg + geom_hline(yintercept = hline)
  
  if (!is.null(fill)){
    gg <- gg +
      geom_jitter_interactive(aes(fill = fill, tooltip = tooltip, data_id = data_id), pch = 21, size = 2, width = 0.1) +
      scale_fill_brewer(fill, palette = "RdBu", direction = -1)
  } 
  
 gg <- gg +
    theme_bw() +
    ggeasy::easy_rotate_x_labels(angle = -45) +
    labs(y = ylabel, title = main_title)
  
  gg
  
}


ratioplot_interactive <- function(...){
  
  gg <- ratioplot(...)
  
  gg
  
  ggr <- girafe(ggobj = plot_grid(gg + guides(fill = "none") + labs(subtitle = "Medians, ordinary scale"),
                                  gg + scale_y_log10() + labs(subtitle = "Medians, log scale"),
                                  rel_widths = c(1,1.35)),
                width_svg = 10, height_svg = 4)
  
  ggr <- girafe_options(ggr, opts_hover(css = "fill:wheat;stroke:orange;r:5pt;") )
  
  ggr
  
}



if (FALSE){
  
  dat_eqsplot <- dat_plot %>% 
    ungroup() %>%
    filter(MYEAR == 2021) %>%
    mutate(
      Tooltip_text = paste0(
        "Conc.: ", Value, " (Min-max: ", Value_min, "-", Value_max, ")"),
      EQS_ratio_min = Value_min/EQS,
      EQS_ratio_max = Value_max/EQS)
  
  ratioplot(data = dat_eqsplot,
                        x = "Station", y = "EQS_ratio", fill = "Region", 
                        tooltip = "Tooltip_text", data_id = "STATION_CODE",
                        ymin = "EQS_ratio_min", ymax = "EQS_ratio_max")  
  
  ratioplot_interactive(data = dat_eqsplot,
                        x = "Station", y = "EQS_ratio", fill = "Region", 
                        tooltip = "Tooltip_text", data_id = "STATION_CODE",
                        ymin = "EQS_ratio_min", ymax = "EQS_ratio_max")  

}




#
# Results for time series of single parameter ----
#

parameter_median_table <- function(..., tooltip = TRUE){
  if (tooltip){
    parameter_median_table_tooltip(...)
  } else {
    parameter_median_table_static(...)
  }
}



parameter_median_table_data <- function(parameter, data_medians, fill, 
                                        series_lasting_until = 2021, 
                                        min_year = 1995){
  
  if (length(min_year) > 1){
    stop("Several years given. Set 'min_year' to be a single year")
  }
  
  data_medians$fill <- data_medians[[fill]]
  fill_column <- fill
  
  dat_plot <- data_medians %>%
    # Select parameter
    dplyr::filter(PARAM %in% parameter) %>%
    # Keep only series lasting until 'series_lasting_until'  
    group_by(Station2) %>%
    mutate(MYEAR_max = max(MYEAR)) %>%
    ungroup() %>%
    dplyr::filter(MYEAR_max >= series_lasting_until) %>%
    # Drop years before "min_year"
    dplyr::filter(MYEAR >= min_year) %>%
    # Station levels
    arrange(desc(Station2)) %>%
    mutate(Station2 = droplevels(Station2))
  
  
  # fill_min <- floor(1000*min(dat_plot$fill, na.rm = T))/1000
  # fill_max <- ceiling(max(dat_plot$fill, na.rm = T))
  fill_min <- 0.0001
  fill_max <- 1000
  
  dat_plot <- dat_plot %>% 
    mutate(
      PARAM = forcats::fct_inorder(PARAM),
      fill_cut = cut(fill, breaks = c(fill_min,0.5,0.75,0.9,1,2,3,5,10,fill_max)),
      VALUE_WW_txt = paste0(
        fill_column, ": ", round(fill, 3), "<br>",
        "Median: ", LOQ_label, round(VALUE_WW_med, 4), " ug/kg<br>",
        "(", round(VALUE_WW_min, 4), "-", round(VALUE_WW_max, 4), "; N =", N, ")")) %>%
    select(Proref_ratio_WW, VALUE_WW_txt, MYEAR, Station2, PARAM, fill, fill_cut,
           Above_EQS, VALUE_WW_med, LOQ_label)
  
  if (nrow(dat_plot) == 0)
    stop("No data selected with given arguments")
  
  dat_plot
  
}

if (F){
  
  # debugonce(parameter_median_table_data)
  parameter_median_table_data("PB", dat_median_fish, fill = "Proref_ratio_WW")
  
}


parameter_median_table_colors <- function(plotdata){
  n_levels <- length(levels(plotdata$fill_cut))
  cols <- c(RColorBrewer::brewer.pal(6, "Blues")[5:2],
            RColorBrewer::brewer.pal(6, "YlOrRd")[1:5])
  
  # Note: aalternative color scales
  #scale_fill_viridis_b(trans = "log10", breaks = c(0.01,1,2,3,5,10,100), option = "plasma") +
  #scale_fill_binned(breaks = c(0.01,1,2,3,5,10,100)) +
  
  if (length(cols) != n_levels){
    cat("\nLevels of fill variable:", levels(plotdata$fill_cut), "\n")
    stop("Data has ", n_levels, " levels (see above) but ", length(cols), " colors given")
  }
  names(cols) <- levels(plotdata$fill_cut)
  cols
}

if (F){
  # debugonce(parameter_median_table_tooltip)
  X <- parameter_median_table_data("HG", dat_median_mussel, fill = "EQS_ratio_WW")
  parameter_median_table_colors(X)
}


# "Dynamic" plot, i.e. with tooltips (returns htmlwidget / girafe object)  
parameter_median_table_tooltip <- function(parameter, data_medians, fill, 
                                           series_lasting_until = 2021, 
                                           min_year = 1995,
                                           show_medians = FALSE,
                                           width_svg = 6, height_svg = 3.5){
  
  fill_column <- fill
  
  dat_plot <- parameter_median_table_data(
    parameter = parameter, data_medians = data_medians, 
    fill = fill, series_lasting_until = series_lasting_until,
    min_year = min_year)
  
  cols <- parameter_median_table_colors(dat_plot)
  
  if (show_medians){
    p <- ggplot(dat_plot, aes(MYEAR, Station2, tooltip = VALUE_WW_txt)) + 
      geom_tile(data = subset(dat_plot, Above_EQS %in% "Over"),
                color = "red", size = 1, height = 0.9, width = 0.9) +
      geom_tile(aes(fill = fill_cut), width = 0.9, height = 0.9) +
      geom_text(aes(label = LOQ_label), size = 1.5, nudge_y = 0.2) +
      geom_text_interactive(aes(label = round(VALUE_WW_med, 3)), nudge_y = -0.1, size = 1.5)
  } else {
    p <- ggplot(dat_plot, aes(MYEAR, Station2, tooltip = VALUE_WW_txt)) + 
      geom_tile(data = subset(dat_plot, Above_EQS %in% "Over"),
                color = "red", size = 1, height = 0.9, width = 0.9) +
      geom_tile_interactive(aes(fill = fill_cut), width = 0.9, height = 0.9)
  }
  
  p <- p +
    scale_fill_manual(fill_column, values = cols) +
    scale_y_discrete(limits = levels(dat_plot$Station2)) +
    theme_bw() +
    theme(
      axis.title = element_blank(),
      axis.text = element_text(size = 7),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8),
      axis.text.x = element_text(angle = -45, hjust = 0),
      panel.grid = element_blank()) 
  
  girafe(ggobj = p, height_svg = 3)
  
}

if (F){
  # debugonce(parameter_median_table_tooltip)
  parameter_median_table_tooltip("HG", dat_median_mussel, fill = "EQS_ratio_WW")
  parameter_median_table_tooltip("HG", dat_median_mussel, fill = "EQS_ratio_WW", 
                                 show_medians = TRUE)
  parameter_median_table_tooltip("PB", dat_median_fish, fill = "Proref_ratio_WW")
}


# Static plot (returns ordinary ggplot object)  
parameter_median_table_static <- function(parameter, data_medians, fill, 
                                          series_lasting_until = 2021, 
                                          min_year = 1995,
                                          show_medians = FALSE){
  
  fill_column <- fill
  
  dat_plot <- parameter_median_table_data(
    parameter = parameter, data_medians = data_medians, 
    fill = fill, series_lasting_until = series_lasting_until,
    min_year = min_year)
  
  cols <- parameter_median_table_colors(dat_plot)
  
  gg <- ggplot(dat_plot, aes(MYEAR, Station2, fill = fill)) +
    geom_tile()
  gg <- gg +
    geom_tile(data = subset(dat_plot, Above_EQS %in% "Over"),
              color = "red", size = 1, height = 0.9, width = 0.9) +
    geom_text(aes(label = round(VALUE_WW_med, 3)), nudge_y = -0.1, size = 3) +
    geom_text(aes(label = LOQ_label), size = 3, nudge_y = 0.3) +
    scale_fill_manual(fill, values = cols) +
    scale_color_manual(values = c("red", "white")) +
    scale_alpha_manual(values = c(1, 0)) +
    scale_y_discrete() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = -45, hjust = 0))
  labs(
    title = "Medians"
  )
  
  gg
  
}


if (F){
  # debugonce(parameter_median_table_static)
  parameter_median_table_static("HG", dat_median_fish, fill = "Proref_ratio_WW")
}
