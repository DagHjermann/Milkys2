

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
# - then k_max and k_values are added after JAGS has run (creating 'results_all_s'),
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
  
  # Get the selected 'data_series' row from the given series number, using column 'series_no' in data_series
  # - 'seriesno' is used to make the file name of the result file
  # Before, seriesno was just the row number of data_series
  # - worked nicely the first time the run is done 
  # The first time an anlysis is done, 'series_no' can be just be se to 1... and up
  # - but it can also be picked series if some series needs to be reestimated
  # - this makes it possible to remake particular result files
  
  if (!"series_no" %in% names(data_series)){
    stop("'data_series' must contain a column called 'series_no' (decides name of result file that will be (over)written) \nSee comments on code")
  }
  
  # Get the selected 'data_series' row 
  data_series_sel <- data_series %>%
    filter(series_no %in% seriesno) %>%
    as.data.frame()
  
  if (nrow(data_series_sel) == 0){
    stop("No 'data_series' row fits the given series number (column 'series_no'")
  } else if (nrow(data_series_sel) >= 2){
    stop(nrow(data_series_sel), "rows of 'data_series' row fits the given series number (column 'series_no'")
  }
  
  # Save metadata already here - in case the jags part crashes  
  result_metadata <- list(
    seriesno = seriesno,
    PARAM = data_series_sel$PARAM, 
    STATION_CODE = data_series_sel$STATION_CODE, 
    TISSUE_NAME = data_series_sel$TISSUE_NAME,
    LATIN_NAME = data_series_sel$LATIN_NAME
  )
  if (!is.null(save_path))
    saveRDS(result_metadata, save_path)
  
  data <- subset(data, 
                 PARAM %in% data_series_sel$PARAM & 
                   STATION_CODE %in% data_series_sel$STATION_CODE & 
                   TISSUE_NAME %in% data_series_sel$TISSUE_NAME &
                   LATIN_NAME %in% data_series_sel$LATIN_NAME)
  
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
  
  # Normally 'k_values' is set from 'k_max' 
  if (is.null(k_values)){
    k_max <- data_series_sel$k_max
    k_values <- 1:k_max
  } 

  last_year_over_LOQ <- data_series_sel$Last_year_over_LOQ
  
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
  
  k_values <- k_values[ok]
  result_run <- list(
    k_max = k_max,
    k_values = k_values
  )
  
  # Add 'result_analysis' to the metadata and overwrite the list
  result <- append(result_metadata, result_run)
  if (!is.null(save_path))
    saveRDS(result, save_path)
  
  if (sum(ok) > 0){
    
    results_all <- results_all_s$result[ok]
    names(results_all) <- k_values
    
    # DIC values and dDIC
    DIC <- purrr::map_dbl(results_all, "dic")
    dDIC <- DIC - min(DIC)
    k_sel_dic <- which.min(DIC)
    dDIC_min <- sort(DIC)[2] - sort(DIC)[1]
    
    # Sum deviance and difference between deviances
    dev <- map(results_all, "deviance") %>% map_dbl(sum) 
    ddev <- -diff(dev)
    # Likelihood ratio test (= difference between deviances)
    df <- diff(k_values)
    p_values <- map2_dbl(ddev, df, ~1-pchisq(.x, .y))
    lr_table = data.frame(k = names(dev), dev=dev, ddev = c(NA,ddev), p = c(NA,p_values))
    
    # Forward selection based on p-values  
    if (sum(p_values > 0.05) > 0){
      model_sel <- which(p_values > 0.05)[1]  # select the last model before the first P > 0.05
      k_sel_forward <- k_values[model_sel]
    } else {
      k_sel_forward <- tail(k_values, 1)
    }
    
    # Largest k which is better (P < 0.05) than all models with lower k
    # Make matrix of p-values (i = null hypothesis, j = alt. hypothesis)
    n <- length(dev)
    ddev_matrix <- matrix(NA, nrow = n, ncol = n)
    k_matrix <- matrix(NA, nrow = n, ncol = n)
    p_matrix <- matrix(NA, nrow = n, ncol = n)
    for (i in 1:(n-1)) { 
      for (j in (i+1):n) { 
        ddev_matrix[i,j] <- dev[i] - dev[j]
        k_matrix[i,j] <- k_values[j] - k_values[i] 
        p_matrix[i,j] <- 1 - pchisq(ddev_matrix[i,j], k_matrix[i,j]) 
      }}
    # Largest p for every alternative hypothesis
    p_max <- apply(p_matrix, 2, max, na.rm = TRUE)
    # Largest k which fulfills (largest p < 0.05)
    k_sel <- max(k_values[which(p_max < 0.05)])

    result_analysis <- list(
      k_values = k_values,
      DIC = DIC,
      dDIC = dDIC,
      dDIC_min = dDIC_min,
      lr_table = lr_table,
      ddev_matrix = ddev_matrix,
      k_matrix = k_matrix,
      p_matrix = p_matrix,
      k_sel = k_sel,
      k_sel_forward = k_sel_forward,
      k_sel_dic = k_sel_dic,
      plot_data = map(results_all, "plot_data"),
      diff_data = map(results_all, "diff_data")
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

extract_modelfit_data <- function(seriesno, folder, data_series, selection = "k_sel"){
  
  fn <- sprintf("trend_%04.0f.rda", seriesno)
  resultlist <- readRDS(paste0(folder, "/", fn))
  if (!is.null(resultlist$plot_data)){
    k_sel <- resultlist[[selection]]
    result <- data.frame(
      PARAM = resultlist[["PARAM"]],
      STATION_CODE = resultlist[["STATION_CODE"]],
      TISSUE_NAME = resultlist[["TISSUE_NAME"]],
      LATIN_NAME = resultlist[["LATIN_NAME"]],
      resultlist$plot_data[[as.character(k_sel)]]
    )
  } else {
    result <- data.frame(
      PARAM = resultlist[["PARAM"]],
      STATION_CODE = resultlist[["STATION_CODE"]],
      TISSUE_NAME = resultlist[["TISSUE_NAME"]],
      LATIN_NAME = resultlist[["LATIN_NAME"]]
    )
  }
  result
}

# Eaxmple of failed model = 2
# $ seriesno    : int 890
# $ PARAM       : chr "HG"
# $ STATION_CODE: chr "24B"
# $ TISSUE_NAME : chr "Muskel"
# $ LATIN_NAME  : chr "Gadus morhua"

if (FALSE){
  debugonce(extract_modelfit_data)
  extract_modelfit_data(58, "Data/125_results_2021_04")  %>% head(5)
  extract_modelfit_data(58, "Data/125_results_2021_04", selection = "k_sel_dic")  %>% head(5)
}

extract_difference_data <- function(seriesno, folder, selection = "k_sel"){
  
  fn <- sprintf("trend_%04.0f.rda", seriesno)
  resultlist <- readRDS(paste0(folder, "/", fn))
  fn <- sprintf("trend_%04.0f.rda", seriesno)
  resultlist <- readRDS(paste0(folder, "/", fn))
  if (!is.null(resultlist$plot_data)){
    k_sel <- resultlist[[selection]]
    result <- data.frame(
      PARAM = resultlist[["PARAM"]],
      STATION_CODE = resultlist[["STATION_CODE"]],
      TISSUE_NAME = resultlist[["TISSUE_NAME"]],
      LATIN_NAME = resultlist[["LATIN_NAME"]],
      resultlist$diff_data[[as.character(k_sel)]]
    )
  } else {
    result <- data.frame(
      PARAM = resultlist[["PARAM"]],
      STATION_CODE = resultlist[["STATION_CODE"]],
      TISSUE_NAME = resultlist[["TISSUE_NAME"]],
      LATIN_NAME = resultlist[["LATIN_NAME"]]
    )
  }
  
  result
  
}

if (FALSE){
  extract_difference_data(58, "Data/125_results_2021_04")
  extract_difference_data(58, "Data/125_results_2021_04", selection = "k_sel_dic")
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
  
  # titlestring <- paste0(resultlist$PARAM, " (", resultlist$Basis, ") at ", resultlist$STATION_CODE, " (", resultlist$TISSUE_NAME, " from ", resultlist$LATIN_NAME, ")")
  titlestring <- paste0(resultlist$PARAM, " at ", resultlist$STATION_CODE, " (", resultlist$TISSUE_NAME, " from ", resultlist$LATIN_NAME, ")")
  
  if (!is.null(resultlist$plot_data)){
    k_sel <- resultlist$k_sel
    gg <- ggplot(resultlist$plot_data[[k_sel]], aes(x, y)) +
      geom_ribbon(aes(ymin = y_q2.5, ymax = y_q97.5), fill = "lightblue") +
      geom_point(data = df_points %>% filter(!is.na(y))) +
      geom_point(data = df_points %>% filter(!is.na(threshold)), aes(y = threshold), shape = 6) +
      geom_line() +
      labs(title = titlestring)
  } else {
    df_points <- df_points %>%
      mutate(y_comb = ifelse(is.na(y), threshold, y))
    gg <- ggplot(df_points, aes(x, y_comb)) +
      geom_point(data = df_points %>% filter(!is.na(y))) +
      geom_point(data = df_points %>% filter(!is.na(threshold)), aes(y = threshold), shape = 6) +
      labs(title = titlestring)
  }
  
  gg
  
}

#
# Get series number from PARAM + STATION_CODE (and more if necessary)
#
get_seriesno <- function(param, stationcode,
                         tissue = NULL,
                         species = NULL,
                         data_series = dat_series_trend){
  
  sel <- with(data_series, PARAM %in% param & STATION_CODE %in% stationcode)
  
  if (!is.null(tissue))
    sel <- sel & with(data_series, TISSUE_NAME %in% tissue)
  
  if (!is.null(species))
    sel <- sel & with(data_series, LATIN_NAME %in% species)
  
  if (sum(sel) == 0){
    stop("No time series found")
  }
  
  if (sum(sel) > 1){
    cat("Values of TISSUE_NAME found:", unique(data_series$TISSUE_NAME), "\n")
    cat("Values of LATIN_NAME found:", unique(data_series$LATIN_NAME), "\n")
    warning(sum(sel), " time series found in data. You may want to specify 'tissue' and/or 'species'.")
  }
  
  which(sel)
  
}

#
# Extract and plot data from results (on files) and data (in memory)
#
tsplot_param <- function(param, stationcode,
                         tissue = NULL,
                         species = NULL,
                         folder,
                         data = dat_all_prep3, 
                         data_series = dat_series_trend){
  
  seriesno <- get_seriesno(
    param = param, 
    stationcode = stationcode,
    tissue = tissue,
    species = species,
    data_series = data_series)
    
  if (length(seriesno) > 1){
    stop(">1 series selected")
  }
  
  gg <- tsplot_seriesno(seriesno,
                  folder = folder,
                  data = data, 
                  data_series = data_series)
  
  gg
  
}

if (FALSE){
  
  debugonce(tsplot_param)
  debugonce(tsplot_seriesno)
  tsplot_param("CB153", "11X", folder = folder_results)
  
}

