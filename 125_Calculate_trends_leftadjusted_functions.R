

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# get_splines_results
#
# Get trend results from data  
# - Note: not used in practice. See 'get_splines_results_seriesno' below
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

get_splines_results <- function(data, 
                                raftery = TRUE, 
                                concentration = "VALUE_WW",
                                save_path = NULL){
  
  # data must contain variables
  #   x, y, threshold, uncensored
  #   k_max

  if (length(unique(data$PARAM)) > 1){
    stop("More than one PARAM in data")
  }
  
  if (length(unique(data$STATION_CODE)) > 1){
    stop("More than one STATION_CODE in data")
  }
  
  if (length(unique(data$TISSUE_NAME)) > 1){
    stop("More than one TISSUE_NAME in data")
  }
  
  if (length(unique(data$LATIN_NAME)) > 1){
    stop("More than one LATIN_NAME in data")
  }

  k_values <- 1:data_prep$k_max[1]
  
  results_all <- purrr::map(
    k_values, 
    ~leftcensored::lc_fixedsplines_tp(
      data = data_prep, 
      k = .x, 
      normalize = FALSE, raftery = raftery, measurement_error = "Uncertainty", 
      predict_x = seq(min(data_prep$x), max(data_prep$x), by = 0.25), 
      compare_with_last = TRUE)
  )
  
  names(results_all) <- k_values
  
  # DIC values and 
  DIC <- purrr::map_dbl(results_all, "dic")
  dDIC <- DIC - min(DIC)
  dDIC_min <- sort(DIC)[2] - sort(DIC)[1]
  
  result <- list(
    PARAM = data$PARAM[1],
    STATION_CODE = data$STATION_CODE[1],
    DIC = DIC,
    dDIC = dDIC,
    dDIC_min = dDIC_min,
    k_sel = k_values[which.min(DIC)],
    plot_data = results_all[[which.min(DIC)]]$plot_data,
    diff_data = results_all[[which.min(DIC)]]$diff_data
  )
  
  if (!is.null(save_path))
    saveRDS(result, save_path)
  
  result
  
}

if (FALSE){
  
  # test
  
  param <- "CB28"
  station <- "36A"
  
  debugonce(get_splines_results)
  test <- get_splines_results(
    subset(dat_all, PARAM %in% param & STATION_CODE %in% station))
}


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# get_splines_results_seriesno
#
# Get trend results from series number  
# - This is the one actually used  
#
# Runs analysis for the time series given by row number 'seriesno' in 'data_series'
# Data are written as a list to an rds file, 3 times:
# - first, just after the function has started, just the parameter name etc. are written ('result_metadata')
# - then k_max and k_values_ok are added after JAGS has run (creating 'results_all_s'),
#   and the first file is overwritten 
# - then, if JAGS worked for at least one of the k_values, actual results are added to the list,
#   and the first file is overwritten yet again 
# - The reason for this is to be able to tell whether JAGS returned no usable results, or whether
#   the procedure was interrupted (e.g. by the user)
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o



# Note: For "normal" use, don't set k_values and filter_rule2. 
# - the data should be filtered for rule 2 and the analysis should go through k = 1 to k_max, then let DIC decide
# - however, if Rule2 is FALSE for the first observation (so it matters) and k = 1 is selected as the best
#   model, the model can be refitted for k = 1 without filter for rule 2, 
#   by setting k_values = 1 and filter_rule2 = FALSE.

get_splines_results_seriesno <- function(seriesno, 
                                         data, 
                                         data_series, 
                                         foldername, 
                                         raftery = TRUE,
                                         k_values = NULL,     # k_values should normally not be set 
                                         filter_rule2 = TRUE  # should normally be TRUE
                                         ){   
  
  # data must contain variables
  #   x, y, threshold, uncensored
  #   k_max
  
  save_path <- paste0(foldername, "/", sprintf("trend_%04i.rda", seriesno))
  
  # Save metadata already here - in case the jags part crashes  
  result_metadata <- list(
    seriesno = seriesno,
    PARAM = data_series$PARAM[seriesno], 
    STATION_CODE = data_series$STATION_CODE[seriesno], 
    TISSUE_NAME = data_series$TISSUE_NAME[seriesno],
    LATIN_NAME = data_series$LATIN_NAME[seriesno]
  )
  if (!is.null(save_path))
    saveRDS(result_metadata, save_path)
  
  data <- subset(data, 
         PARAM %in% data_series$PARAM[seriesno] & 
           STATION_CODE %in% data_series$STATION_CODE[seriesno] & 
           TISSUE_NAME %in% data_series$TISSUE_NAME[seriesno] &
           LATIN_NAME %in% data_series$LATIN_NAME[seriesno])

  if (length(unique(data$PARAM)) > 1){
    stop("More than one PARAM in data")
  }
  
  if (length(unique(data$STATION_CODE)) > 1){
    stop("More than one STATION_CODE in data")
  }
  
  if (length(unique(data$TISSUE_NAME)) > 1){
    stop("More than one TISSUE_NAME in data")
  }
  
  if (length(unique(data$LATIN_NAME)) > 1){
    stop("More than one LATIN_NAME in data")
  }
  
  # Normally 
  if (is.null(k_values)){
    k_max <- pull(data_series[seriesno,], k_max)
    k_values <- 1:k_max
  } 
  
  last_year_over_LOQ <- pull(data_series[seriesno,], Last_year_over_LOQ)
  
  # Rule 1. Time series should be truncated from the left until Nplus/N >= 0.5     
  # Rule 2. If a linear/smooth trend is fitted, the first year must be non-censored   
  #   - however, to be able to compare models using DIC, all data need to be of same length        
  data_clean <- data %>%
    filter(Rule1)
  
  if (filter_rule2){
    data_clean <- data_clean %>%
      filter(Rule2)
  }
  
  lc_fixedsplines_tp_s <- safely(leftcensored::lc_fixedsplines_tp)

  results_all_s <- purrr::map(
    k_values, 
    ~lc_fixedsplines_tp_s(
      data = data_clean, 
      k = .x, 
      normalize = FALSE, 
      raftery = raftery, 
      measurement_error = "Uncertainty", 
      predict_x = seq(min(data$x), max(data$x), by = 0.25), 
      reference_x = last_year_over_LOQ, 
      set_last_equal_x = last_year_over_LOQ)
  )
  
  results_all_s <- transpose(results_all_s)
  ok <- map_lgl(results_all_s$error, is.null)

  result_run <- list(
    k_max = k_max,
    k_values_ok = k_values[ok]
  )

  # Add 'result_analysis' to the metadata and overwrite the list
  result <- append(result_metadata, result_run)
  if (!is.null(save_path))
    saveRDS(result, save_path)
  
  if (sum(ok) > 0){
    
    results_all <- results_all_s$result[ok]
    names(results_all) <- k_values[ok]
    
    # DIC values and dDIC
    DIC <- purrr::map_dbl(results_all, "dic")
    dDIC <- DIC - min(DIC)
    dDIC_min <- sort(DIC)[2] - sort(DIC)[1]
    
    result_analysis <- list(
      DIC = DIC,
      dDIC = dDIC,
      dDIC_min = dDIC_min,
      k_sel = k_values[which.min(DIC)],
      plot_data = results_all[[which.min(DIC)]]$plot_data,
      diff_data = results_all[[which.min(DIC)]]$diff_data
    )
    
    # Add 'result_analysis' to the metadata and overwrite the list
    result <- append(result, result_analysis)
    if (!is.null(save_path))
      saveRDS(result, save_path)
    
  }
  
  NULL
  
}

if (FALSE){
  
  # test
  
  param <- "CB28"
  station <- "36A"
  
  debugonce(get_splines_results)
  test <- get_splines_results(
    subset(dat_all, PARAM %in% param & STATION_CODE %in% station))
}

# "Safe" version
get_splines_results_seriesno_s <- safely(get_splines_results_seriesno)



#
# Extract raw data (point data)
#

extract_raw_data <- function(seriesno,
                             data = dat_all_prep3, 
                             data_series = dat_series_trend){
  subset(data, 
         PARAM %in% data_series$PARAM[seriesno] & 
           STATION_CODE %in% data_series$STATION_CODE[seriesno] & 
           TISSUE_NAME %in% data_series$TISSUE_NAME[seriesno] &
           LATIN_NAME %in% data_series$LATIN_NAME[seriesno])
  
}

extract_modelfit_data <- function(seriesno, folder, data_series){
  
  fn <- sprintf("trend_%04.0f.rda", seriesno)
  resultlist <- readRDS(paste0(folder, "/", fn))
  data.frame(
    PARAM = resultlist[["PARAM"]],
    STATION_CODE = resultlist[["STATION_CODE"]],
    TISSUE_NAME = resultlist[["TISSUE_NAME"]],
    LATIN_NAME = resultlist[["LATIN_NAME"]],
    resultlist$plot_data
  )

}

if (FALSE){
  debugonce(extract_modelfit_data)
  extract_modelfit_data(187)
}

extract_difference_data <- function(seriesno, folder, data_series){
  
  fn <- sprintf("trend_%04.0f.rda", seriesno)
  resultlist <- readRDS(paste0(folder, "/", fn))
  data.frame(
    PARAM = resultlist[["PARAM"]],
    STATION_CODE = resultlist[["STATION_CODE"]],
    TISSUE_NAME = resultlist[["TISSUE_NAME"]],
    LATIN_NAME = resultlist[["LATIN_NAME"]],
    resultlist$diff_data
  )
  
}

if (FALSE){
  extract_difference_data(58)
}


#
# Extract and plot data from results (on files) and data (in memory)
#
tsplot_seriesno <- function(seriesno,
                            folder,
                            data = dat_all_prep3, 
                            data_series = dat_series_trend){
  
  fn <- sprintf("trend_%04.0f.rda", seriesno)
  resultlist <- readRDS(paste0(folder, "/", fn))
  
  # str(resultlist, 1)
  
  # debugonce(get_pointdata)
  df_points <- extract_raw_data(seriesno, data = data, data_series = data_series)
  
  ggplot(resultlist$plot_data, aes(x, y)) +
    geom_ribbon(aes(ymin = y_q2.5, ymax = y_q97.5), fill = "lightblue") +
    geom_point(data = df_points %>% filter(!is.na(y))) +
    geom_point(data = df_points %>% filter(!is.na(threshold)), aes(y = threshold), shape = 6) +
    geom_line()
  
  
}


