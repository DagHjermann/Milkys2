

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# get_splines_results
#
# Get trend results from data  
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

  data_prep <- lc_prepare(data,
                          x = "MYEAR",
                          y = concentration, 
                          censored = "FLAG1",
                          log = TRUE,
                          keep_original_columns = TRUE)
  
  k_values <- 1:data_prep$k_max[1]
  
  results_all <- purrr::map(
    k_values, 
    ~leftcensored::lc_fixedsplines_tp(
      data = data_prep, 
      k = .x, 
      normalize = FALSE, raftery = raftery, measurement_error = "Uncertainty", 
      predict_x = seq(min(data_test_prep$x), max(data_test_prep$x)), 
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
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

get_splines_results_seriesno <- function(i, data, data_series, raftery = TRUE){
  
  file_no <- i   # to avoid using 'i' inside sprintf (where it has a special meaning)
  
  get_splines_results(
    subset(data, 
           PARAM %in% data_series$PARAM[i] & 
             STATION_CODE %in% data_series$STATION_CODE[i] & 
             TISSUE_NAME %in% data_series$TISSUE_NAME[i]), 
    raftery = raftery, 
    save_path = sprintf("Data/124_results/trend_%04i.rda", i)
  )
  
}

# "Safe" version
get_splines_results_seriesno_s <- safely(get_splines_results_seriesno)


