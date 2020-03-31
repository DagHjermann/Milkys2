# Copied from '16_Trend_functions.R' in Milkys_2018

#
# NOTE: YEAR changed to MYEAR
#

#
# Functions
#

# pick_data_yr                Picks data from year1 to year2
# models_yr                   Picks data from year1 to year2 and models it with both non-linear and linear models
# p_linear                    Returns p-value for increase using linear model 
# p_nonlinear                 Returns p-value for increase using non-linear model (ie., last minus first point) 
# p_best_AIC                  Returns best p-value (non-linear or linear), as well as AIC for non-linear and linear
# theme_custom                ggplot adjustments 
# get_plot_data               The "extract data" part of plot_all 
# get_medians_all_stations
# get_medians_for_regression 
# select_for_regr_a
# select_for_regr_b
# calc_models
# calc_models_one_station
# data_for_excel 
# statistics_for_excel 
# select_model
# add_model_fits 
# calc_models_all_stations
# write_multiple_determ_to_file
# write_multiple_determ_to_dataframe
# plot_object 
# figure_dims
# plot_all_stations 
# calc_models_several_determ 
# make_localformat_csv 

# How 'calc_models_several_determ' works:
#
# calc_models_several_determ 
# - for each determinand, calls 
#   'calc_models_all_stations'
#   - runs 'get_medians_all_stations' 
#   - for each station, calls 
#     'calc_models_one_station'
#     - runs 'get_medians_for_regression'
#     - then runs 'select_for_regr_a()' and 'select_for_regr_b'
#   - results in a list of objects

# model_from_leveldata
# - picks data from 'rawdata'
# - extracts median
# - calls 'calc_models_one_station2()'
#     - runs 'select_for_regr_a()' and 'select_for_regr_b'
#     - runs 'calc_models'
#     - runs 'add_model_fits'
#     - runs 'statistics_for_excel'
# - the result is 'result_object' containing 'statistics_for_file' and  'data_object'
#     - if calc_models_one_station2 fails, 'statistics_for_excel_empty' is run to result in 'result_stat'
# - picks 'statistics_for_file' from this (calls it 'result_stat'), adds PARAM, LATIN_NAME etc. and returns this as a one-line dataframe

#
# pick_data_yr
# Picks data from year1 to year2
# Changes variable name sto 'x' and 'y'
# Deletes observations lacking y values
#
pick_data_yr <- function(data, year1, year2, var_x = "Year", var_y = "log_HG"){
  sel <- data[,var_x] >= year1 & data[,var_x] <= year2
  df <- data[sel, c(var_x, var_y)]
  colnames(df) <- c("x", "y")
  df <- df[!is.na(df$y),]
  df
}

#
# Picks data from year1 to year2 and models it with both non-linear and linear models
# Returns list with both data, models and AIC
#
models_yr <- function(data, year1, year2, ...){
  data_pick <- pick_data_yr(data=data, year1, year2, ...)
  mod_nonlin <- OSPAR_Trend_Analysis(dataset = data_pick)
  # options(show.error.messages = FALSE)
  mod_lin <- lm(y~x, data = data_pick)
  # options(show.error.messages = TRUE)
  list(AIC = c(nonlinear = AIC_nonlinear(data_pick, mod_nonlin),
               linear = AIC_linear(data_pick, mod_lin)),
       AICc = c(nonlinear = AICc_nonlinear(data_pick, mod_nonlin),
                linear = AICc_linear(data_pick, mod_lin)),
       mod_nonlin = mod_nonlin,
       mod_lin = mod_lin,
       data = data_pick)
}

#
# Returns p-value for increase using non-linear model (ie., last minus first point) 
#
p_nonlinear <- function(model){
  nlin_pred <- model$yFit
  first_point <- head(nlin_pred, 1)
  last_point <- tail(nlin_pred, 1)
  diff <- last_point$Estimate - first_point$Estimate
  s_comb <- sqrt(first_point$Variance + last_point$Variance)
  (1-pt(diff/s_comb, 1))/2
}

#
# Returns p-value for increase using linear model (ordinary p-value) 
#
p_linear <- function(model){
  summary(model)$coef[2,4]
}

#
# Returns best p-value (non-linear or linear), as well as AIC for non-linear and linear
# AIC values tells you which test that was used
#
p_best_AIC <- function(data, year1, year2, ...){
  models <- models_yr(data, year1, year2, ...)
  AIC <- models$AIC
  if (AIC[["nonlinear"]] < AIC[["linear"]]){
    p <- p_nonlinear(models$mod_nonlin)
  } else {
    p <- p_linear(models$mod_nonlin)
  }
  c(p = p, AIC)
}


# ggplot adjustments
theme_custom <- theme(legend.title=element_text(size=rel(1)),
                      legend.text=element_text(size=rel(1)),
                      axis.title=element_text(size=rel(1.2)),
                      axis.title.x=element_text(vjust=-0.2),
                      axis.title.y=element_text(vjust=1.7), 
                      axis.text=element_text(size=rel(1), color="black"),
                      plot.margin = unit(c(1,1,1,1), "cm"))


#
# The "extract data" part of plot_all (no it doesn't plot the data)
# 
get_plot_data <- function(det, sp, tis, variable_name){
  if (variable_name == "VALUE_WW"){
    basis_txt <- "wet weight basis"
  } else if (variable_name == "VALUE_DW"){
    basis_txt <- "dry weight basis"
  } else if (variable_name == "VALUE_FB"){
    basis_txt <- "fat basis"
  }
  df <- get_rawdata(det, sp, tis, variable_name, min_years = 1, min_indiv_per_year = 1)
  df$LOQ <- "Below LOQ"
  df$LOQ[is.na(df$FLAG1)] <- "Above LOQ"
  df$LOQ <- factor(df$LOQ, levels = c("Below LOQ", "Above LOQ"))
  df
}




# will add range per year

get_medians_all_stations <- function(...){
  df <- get_plot_data(...)
  # head(df)
  # xtabs(~STATION_CODE + MYEAR, df)
  # df$MYEAR_f <- factor(df$MYEAR, levels = yrs)
  
  # Medians, all stations
  df %>%
    group_by(STATION_CODE, MYEAR) %>%
    summarise(Median = median(Measurement), N = n(), Over_LOQ = sum(is.na(FLAG1)))
}


# df_med <- get_medians_all_stations("CB118", sp = "Gadus morhua", tis = "Lever", variable_name = "VALUE_WW")

get_medians_for_regression <- function(mediandata, station, yrs){
  # Medians, one station, 'yrs' = years to print (not necessarily for analysis)
  sel <- mediandata$STATION_CODE %in% station & mediandata$MYEAR %in% yrs
  mediandata[sel,]
}

# df_med_stat <- get_medians_for_regression(df_med, station = "30B")

#
# The data frame must have the variable "Over_LOQ" which equals the number of measurements over LOQ
# Returns a list including the original data and "sel_ts" which is the observations picked for regression analysis
#
select_for_regr_a <- function(df_med_st){
  sel_ts <- rep(TRUE, nrow(df_med_st))
  N <- sum(sel_ts)
  Nplus <- sum(df_med_st$Over_LOQ[sel_ts] > 0)
  # cat(which(sel_ts), "N =", N, "Nplus = ", Nplus, "\n")
  i <- 1
  while (Nplus < (N/2)){
    sel_ts[i] <- FALSE
    N <- sum(sel_ts)
    Nplus <- sum(df_med_st$Over_LOQ[sel_ts] > 0)
    i <- i+1
  }
  # cat(which(sel_ts), "N =", N, "Nplus = ", Nplus, "\n")
  while (Nplus >= 5 & df_med_st$Over_LOQ[sel_ts][1] == 0){
    sel_ts[i] <- FALSE
    i <- i+1
  }
  # cat(which(sel_ts), "N =", N, "Nplus = ", Nplus, "\n")
  list(df_med_st = df_med_st, sel_ts = sel_ts, N = N, Nplus = Nplus)
}

#
# The data frame must have the variable "MYEAR"
# Returns same type of list as select_for_regr_a
#
select_for_regr_b <- function(sel_object){
  df_med_st <- sel_object$df_med_st
  sel_ts <- sel_object$sel_ts
  # Rule 3: If yr_last is the last year with over-LOQ data, the years after are set to yr_last
  # We let MYEARS stay unchanged and change MYEAR_regr
  df_med_st$MYEAR_regr <- df_med_st$MYEAR
  df_med_st[sel_ts,]
  index_last_over_LOQ <- tail(which(df_med_st$Over_LOQ > 0), 1)
  # for testing
  # index_last_over_LOQ <- 8
  index_after_last_over_LOQ <- which(sel_ts)[which(sel_ts) > index_last_over_LOQ] 
  index_after_last_over_LOQ
  df_med_st$MYEAR_regr[index_after_last_over_LOQ] <- df_med_st$MYEAR[index_last_over_LOQ] 
  list(df_med_st = df_med_st, sel_ts = sel_ts, N = sel_object$N, Nplus = sel_object$Nplus)
}

GAM_trend_analysis <-  function(dataset){
  max_df <- ifelse(nrow(dataset) <= 14, 3, 4)
  sel <- is.finite(dataset$x) & is.finite(dataset$y) 
  mgam <- mgcv::gam(y ~ s(x, k = max_df + 1), data = dataset[sel,])
  pr <- predict(mgam, se.fit = TRUE)
  dY <- -qt(0.025, nrow(dataset))
  yFit <- data.frame(Year = dataset$x[sel], Estimate = pr$fit, SE = pr$se.fit, LowLimit = pr$fit - dY*pr$se.fit, HighLimit = pr$fit + dY*pr$se.fit)  
  list(model = mgam, yFit = yFit)
}


# for testing
# df_med_st <- data.frame(MYEAR = 2001:2010, Median = seq(10,1), Over_LOQ = c(0,0,0,0,0,1,1,0,1,0))  # rule 1
# df_med_st <- data.frame(MYEAR = 2001:2010, Median = seq(10,1), Over_LOQ = c(0,0,1,1,0,1,0,1,1,0))  # rule 2
# select_for_regr_a(df_med_st)
# select_for_regr_a(df_med_st) %>% select_for_regr_b()

# Calculate both linear and non-linear models
# Based on 'models_yr', but it uses the entire input data for regression
calc_models <- function(obj, var_x = "MYEAR_regr", var_y = "Median", gam = FALSE){
  data <- obj$df_med_st[obj$sel_ts,]
  data_pick <- data[, c(var_x, var_y)]
  data_pick[,2] <- log(data_pick[,2])
  colnames(data_pick) <- c("x", "y")
  if (nrow(data_pick) > 0){
    # options(show.error.messages = FALSE)
    mod_nonlin <- try(OSPAR_Trend_Analysis(dataset = data_pick))
    mod_lin <- lm(y~x, data = data_pick)
    pr <- predict(mod_lin, se = TRUE)
    dY <- -qt(0.025, nrow(data_pick))
    mod_lin_yFit <- data.frame(Year = data_pick$x, Estimate = pr$fit, SE = pr$se.fit, LowLimit = pr$fit - dY*pr$se.fit, HighLimit = pr$fit + dY*pr$se.fit)  
    # options(show.error.messages = TRUE)
    if (class(mod_nonlin)[1] != "try-error"){
      mod_nonlin$yFit <- data.frame(Year = data_pick$x, mod_nonlin$yFit)
      result <- 
        list(AIC = c(nonlinear = AIC_nonlinear(data_pick, mod_nonlin),
                     linear = AIC_linear(data_pick, mod_lin)),
             AICc = c(nonlinear = AICc_nonlinear(data_pick, mod_nonlin),
                      linear = AICc_linear(data_pick, mod_lin)),
             mod_nonlin = mod_nonlin,
             mod_lin = mod_lin,
             mod_lin_yFit = mod_lin_yFit,
             data = data_pick)
    } else {
      result <- list(AIC = c(nonlinear = NA,
                             linear = AIC_linear(data_pick, mod_lin)),
                     AICc = c(nonlinear = NA,
                              linear = AICc_linear(data_pick, mod_lin)),
                     mod_nonlin = NULL,
                     mod_lin = mod_lin,
                     mod_lin_yFit = mod_lin_yFit,
                     data = data_pick)
    }
  } else {
    result <- list(AIC = c(nonlinear = NA,
                           linear = NA),
                   AICc = c(nonlinear = NA,
                            linear = NA),
                   mod_nonlin = NULL,
                   mod_lin = NULL,
                   mod_lin_yFit = mod_lin_yFit,
                   data = data_pick)
  }  
  result
}

# Calculate gam models
# Based on 'models_yr', but it uses the entire input data for regression
calc_models_gam <- function(obj, var_x = "MYEAR_regr", var_y = "Median", gam = FALSE, log = TRUE){
  data <- obj$df_med_st[obj$sel_ts,]
  data_pick <- data[, c(var_x, var_y)]
  if (log)
    data_pick[,2] <- log(data_pick[,2])
  colnames(data_pick) <- c("x", "y")
  if (nrow(data_pick) > 0){
    mod_gam <- try(GAM_trend_analysis(dataset = data_pick))
    mod_lin <- lm(y~x, data = data_pick)
    pr <- predict(mod_lin, se = TRUE)
    dY <- -qt(0.025, nrow(data_pick))
    mod_lin_yFit <- data.frame(Year = data_pick$x[!is.na(data_pick$y)], Estimate = pr$fit, SE = pr$se.fit, LowLimit = pr$fit - dY*pr$se.fit, HighLimit = pr$fit + dY*pr$se.fit)  
    if (class(gam)[1] != "try-error"){
      result <- 
        list(AIC = c(nonlinear = AIC(mod_gam$model),
                     linear = AIC(mod_lin)),
             AICc = c(nonlinear = AICc(mod_gam$model),
                      linear = AICc(mod_lin)),
             mod_nonlin = mod_gam,
             mod_lin = mod_lin,
             mod_lin_yFit = mod_lin_yFit,
             data = data_pick)
    } else {
      result <- 
        list(AIC = c(nonlinear = NA,
                     linear = AIC(mod_lin)),
             AICc = c(nonlinear = NA,
                      linear = AICc(mod_lin)),
             mod_nonlin = NA,
             mod_lin = mod_lin,
             mod_lin_yFit = mod_lin_yFit,
             data = data_pick)
    }
  } else {
    result <- list(AIC = c(nonlinear = NA,
                           linear = NA),
                   AICc = c(nonlinear = NA,
                            linear = NA),
                   mod_nonlin = NULL,
                   mod_lin = NULL,
                   mod_lin_yFit = NULL,
                   mod_gam = NULL,
                   data = data_pick)
  }  
  result
}


# Calculates models for one determninant, one station, given which number of years to attempt to run
calc_models_one_station <- function(medians_all_stations, station, yrs){
  df_med_regr_20yr <- get_medians_for_regression(medians_all_stations, station = station, yrs = yrs)
  df_med_object <- df_med_regr_20yr %>% select_for_regr_a() %>% select_for_regr_b()
  modelresults_20yr <- calc_models(df_med_object)
  df_med_object <- add_model_fits(df_med_object, modelresults_20yr)
  df1 <- data_for_excel(medians_all_stations, station=station)
  df2 <- statistics_for_excel(df_med_object, modelresults_20yr)
  list(data_for_file = df1, statistics_for_file = df2, data_object = df_med_object)
}

# modelresults <- calc_models(df_med_object)


#df_med <- get_medians_all_stations("CB118", sp = "Gadus morhua", tis = "Lever", variable_name = "VALUE_WW")
#df_med_stat <- get_medians_for_regression(df_med, station = "30B")
#df_med_object <- df_med_stat %>% select_for_regr_a() %>% select_for_regr_b()
#modelresults <- calc_models(df_med_object)

data_for_excel <- function(df_med, station, yrs = 1990:2016){
  # For output to table
  df_med_st <- df_med[df_med$STATION_CODE %in% station & df_med$MYEAR %in% yrs,]
  df_med_st$MYEAR_f <- factor(df_med_st$MYEAR, levels = yrs)
  df_med_st[, c("MYEAR_f", "Median")] %>%
    spread(MYEAR_f, Median, drop = FALSE)
}



statistics_for_excel <- function(obj, regr_results, gam = FALSE){
  if (sum(obj$sel_ts) > 0 & !gam){
    df_stat <- data.frame(
      Year1 = min(obj$df_med_st$MYEAR[obj$sel_ts]),
      Year2 = max(obj$df_med_st$MYEAR[obj$sel_ts]),
      N = obj$N,
      Nplus = obj$Nplus,
      Mean = mean(obj$df_med_st$Median, na.rm = TRUE),
      p_linear = p_linear(regr_results$mod_lin),
      p_nonlinear = p_nonlinear(regr_results$mod_nonlin),
      AICc_lin = regr_results$AICc[["linear"]],
      AICc_nonlin = regr_results$AICc[["nonlinear"]],
      Lin_slope = coef(regr_results$mod_lin)[2],
      Lin_yr1 = head(regr_results$mod_lin$fitted, 1),
      Lin_yr2 = tail(regr_results$mod_lin$fitted, 1),
      Nonlin_yr1 = head(regr_results$mod_nonlin$yFit$Estimate, 1),
      Nonlin_yr2 = tail(regr_results$mod_nonlin$yFit$Estimate, 1),
      Over_LOQ_yr2 = tail(obj$df_med_st[obj$sel_ts, "Over_LOQ"], 1)
    )
  } else if (sum(obj$sel_ts) > 0 & gam){
    # Calculate p-value for GAM model (gam_p)
    x1 <- head(regr_results$mod_nonlin$yFit$Estimate,1)
    x2 <- tail(regr_results$mod_nonlin$yFit$Estimate,1)
    SE1 <- head(regr_results$mod_nonlin$yFit$SE,1)
    SE2 <- tail(regr_results$mod_nonlin$yFit$SE,1)
    mean_SE <- sqrt(SE1^2 + SE2^2)
    gam_p <- pt((x2-x1)/mean_SE, nrow(regr_results$mod_nonlin$yFit))
    df_stat <- data.frame(
      Year1 = min(obj$df_med_st$MYEAR[obj$sel_ts]),
      Year2 = max(obj$df_med_st$MYEAR[obj$sel_ts]),
      N = obj$N,
      Nplus = obj$Nplus,
      Mean = mean(obj$df_med_st$Median, na.rm = TRUE),
      p_linear = p_linear(regr_results$mod_lin),
      p_nonlinear = gam_p,
      AICc_lin = regr_results$AICc[["linear"]],
      AICc_nonlin = AICc(regr_results$mod_nonlin$model),
      Lin_slope = coef(regr_results$mod_lin)[2],
      Lin_yr1 = head(regr_results$mod_lin$fitted, 1),
      Lin_yr2 = tail(regr_results$mod_lin$fitted, 1),
      Nonlin_yr1 = head(regr_results$mod_nonlin$yFit$Estimate, 1),
      Nonlin_yr2 = tail(regr_results$mod_nonlin$yFit$Estimate, 1),
      Over_LOQ_yr2 = tail(obj$df_med_st[obj$sel_ts, "Over_LOQ"], 1)
    )
  } else {
    df_stat <- data.frame(
      Year1 = NA,
      Year2 = NA,
      N = obj$N,
      Nplus = obj$Nplus,
      Mean = mean(obj$df_med_st$Median, na.rm = TRUE),
      p_linear = NA,
      p_nonlinear = NA,
      AICc_lin = NA,
      AICc_nonlin = NA,
      Lin_slope = NA,
      Lin_yr1 = NA,
      Lin_yr2 = NA,
      Nonlin_yr1 = NA,
      Nonlin_yr2 = NA,
      Over_LOQ_yr2 = NA
    )
  }
  df_stat$Model_used <- select_model(df_stat)
  if (df_stat$Model_used == "Linear"){
    df_stat$P_change <- df_stat$p_linear
  } else if (df_stat$Model_used == "Nonlinear"){
    df_stat$P_change <- df_stat$p_nonlinear
  } else {
    df_stat$P_change <- NA
  }
  if (df_stat$P_change < 0.05 & df_stat$Model_used == "Linear"){
    df_stat$Dir_change <- ifelse(df_stat$Lin_slope > 0, "Up", "Down")
  } else if (df_stat$P_change < 0.05 & df_stat$Model_used == "Nonlinear"){
    df_stat$Dir_change <- ifelse(df_stat$Nonlin_yr2 > df_stat$Nonlin_yr1, "Up", "Down")
  } else {
    df_stat$Dir_change <- ""
  }
  df_stat
}


select_model <- function(df_statistics){
  if (df_statistics$Nplus <= 1){
    model <- "None"
  } else if (df_statistics$Nplus == 2 & df_statistics$N == 2){
    model <- "None"
  } else if (df_statistics$Nplus %in% 2:4 & df_statistics$N >= 3){
    model <- "Mean"
  } else if (df_statistics$Nplus %in% 5:6){
    model <- "Linear"
  } else if (df_statistics$Nplus >= 7 & !is.na(df_statistics$AICc_nonlin)){
    if (df_statistics$AICc_lin <= df_statistics$AICc_nonlin){
      model <- "Linear"
    } else {
      model <- "Nonlinear"
    }
  } else if (df_statistics$Nplus >= 7 & is.na(df_statistics$AICc_nonlin)){
    model <- "Linear"
  } else {
    model <- "Error"
  }
  model
}


add_model_fits <- function(obj, regr_results, gam = FALSE){
  df <- obj$df_med_st
  df$Fit_lin <- NA
  df$Fit_nonlin <- NA
  median_exists <- is.finite(obj$df_med_st$Median)
  if (!is.null(regr_results$mod_lin$fitted))
    df$Fit_lin[obj$sel_ts][median_exists] <- regr_results$mod_lin$fitted    # the !is.na selection weeds out NAs in the middle of the time series
  if (!is.null(regr_results$mod_nonlin) & !gam)
    df$Fit_nonlin[obj$sel_ts] <- regr_results$mod_nonlin$yFit$Estimate
  if (!is.null(regr_results$mod_nonlin) & gam)
    df$Fit_nonlin[obj$sel_ts][median_exists] <- regr_results$mod_nonlin$yFit$Estimate
  updated_obj <- obj
  updated_obj$df_med_st <- as.data.frame(df)
  updated_obj
}

# df_med_object <- add_model_fits(df_med_object, modelresults_20yr)

# 
# Function that takes a number of determinants (but same species, tissue and basis), 
#   calculates models and 
#   1) writes results to CSV file 
#   2) saves a list data objects, where each object can be plotted using 'plot_object', or all
#      objects can be saved usibng 'plot_all_stations'
#  OLD VERSION (wriring to file)
# 
calc_models_all_stations_OLD <- function(det, sp = "Gadus morhua", tis = "Lever", variable_name = "VALUE_WW", filename = "Temp.csv"){
  df_med <- get_medians_all_stations(det, sp = sp, tis = tis, variable_name = variable_name)
  sts <- unique(df_med$STATION_CODE)
  result_list <- vector("list", length(sts))
  names(result_list) <- sts
  attr(result_list, "det") <- det
  attr(result_list, "sp") <- sp
  attr(result_list, "tis") <- tis
  attr(result_list, "variable_name") <- variable_name
  i <- 1
  st <- sts[i]
  Tfile <- file(filename, "w+")
  result_object <- calc_models_one_station(df_med, st, yrs = 1997:2016)
  df <- result_object$statistics_for_file
  df <- data.frame(Station = st, Species = sp, Tissue = tis, Variable = variable_name, df, stringsAsFactors = FALSE)
  write.table(df, sep = ",", quote = FALSE, col.names = TRUE, row.names = FALSE, file = Tfile)
  result_list[[i]] <- result_object
  for (i in 2:length(sts)){
    st <- sts[i]
    result_object <- calc_models_one_station(df_med, st, yrs = 1997:2016)
    cat(st, ",", sp, ",", tis, ",", variable_name, ",", sep = "", file = Tfile)
    string <- paste(result_object$statistics_for_file[1,], collapse = ",")
    string <- gsub("NA", "", string, fixed = TRUE)
    string <- gsub("NaN", "", string, fixed = TRUE)
    string <- gsub("Inf", "", string, fixed = TRUE)
    string <- gsub("-Inf", "", string, fixed = TRUE)
    cat(string, file = Tfile)
    cat("\n", file = Tfile)
    result_list[[i]] <- result_object
  }
  close(Tfile)
  result_list
}

#
# Function that takes a number of determinants (but same species, tissue and basis), 
#   calculates models and 
#   1) writes results to CSV file 
#   2) saves a list data objects, where each object can be plotted using 'plot_object', or all
#      objects can be saved usibng 'plot_all_stations'
# 
calc_models_all_stations <- function(det, sp = "Gadus morhua", tis = "Lever", variable_name = "VALUE_WW", yrs, trace = FALSE){
  df_med <- get_medians_all_stations(det, sp = sp, tis = tis, variable_name = variable_name)
  sts <- unique(df_med$STATION_CODE)
  result_list <- vector("list", length(sts))
  names(result_list) <- sts
  attr(result_list, "det") <- det
  attr(result_list, "sp") <- sp
  attr(result_list, "tis") <- tis
  attr(result_list, "variable_name") <- variable_name
  for (i in 1:length(sts)){
    st <- sts[i]
    if (trace)   cat(det, i, st, "\n")
    result_object <- try(calc_models_one_station(medians_all_stations=df_med, st=st, yrs = yrs))
    result_list[[i]] <- result_object
  }
  result_list
}

# test <- calc_models_all_stations("CB118", filename = "Results/test2.csv")

# 
# Write object to file
# Doesn't work, and has anyway been replaced b 'write_multiple_determ_to_dataframe' below
# 
write_multiple_determ_to_file <- function(list_of_object_lists, filename = "Temp.csv"){
  N1 <- length(list_of_object_lists)
  for (i in 1:N1){
    obj_list <- list_of_object_lists[[i]]
    N2 <- length(obj_list)
    for (j in 1:N2){
      obj <- obj_list[[j]]
      Tfile <- file(filename, "w+")
      df <- obj$statistics_for_file
      df <- data.frame(Determ = attr(obj_list, "det"), Species = attr(obj_list, "sp"), 
                       Tissue  = attr(obj_list, "tis"), Variable = attr(obj_list, "variable_name"), 
                       Station = names(obj_list)[j], 
                       df, 
                       stringsAsFactors = FALSE)
      if (i == 1 & j == 1){
        write.table(df, sep = ",", quote = FALSE, col.names = TRUE, row.names = FALSE, file = Tfile)
      } else {
        string <- paste(df[1,], collapse = ",")
        string <- paste(result_object$statistics_for_file[1,], collapse = ",")
        string <- gsub("NA", "", string, fixed = TRUE)
        string <- gsub("NaN", "", string, fixed = TRUE)
        string <- gsub("Inf", "", string, fixed = TRUE)
        string <- gsub("-Inf", "", string, fixed = TRUE)
        cat(string, file = Tfile)
        cat("\n", file = Tfile)
      }
    }
  }
  
  close(Tfile)
}

# 
# Write object to dataframe 
# 
write_multiple_determ_to_dataframe <- function(list_of_object_lists){
  # total_length <- list_of_object_lists %>% map_int(length) %>% sum()
  first_record <- TRUE
  N1 <- length(list_of_object_lists)
  for (i in 1:N1){
    obj_list <- list_of_object_lists[[i]]
    N2 <- length(obj_list)
    for (j in 1:N2){
      obj <- obj_list[[j]]
      if (class(obj)[1] != "try-error"){
        df <- obj$statistics_for_file
        df <- data.frame(Determ = attr(obj_list, "det"), Species = attr(obj_list, "sp"), 
                         Tissue  = attr(obj_list, "tis"), Variable = attr(obj_list, "variable_name"), 
                         Station = names(obj_list)[j], 
                         df, 
                         stringsAsFactors = FALSE)
        if (first_record){
          result <- df
          first_record <- FALSE
        } else {
          result <- rbind(result, df)
        }
      }
    }
  }
  result
}

plot_object <- function(obj, xlab = "", ylab = "Median conc"){
  plot(Median~MYEAR, obj$df_med_st, log = "y", xlab = xlab, ylab = ylab)
  lines(exp(Fit_lin)~MYEAR, obj$df_med_st, col = colors[1])
  lines(exp(Fit_nonlin)~MYEAR, obj$df_med_st, col = colors[2])
}

# plot_object(df_med_object)

figure_dims <- function(x) {
  r <- floor(sqrt(x))
  c <- ceiling(x/r)
  c(r,c)
}
# figure_dims(11)

plot_all_stations <- function(object_list, title = NULL){
  sts <- names(object_list)
  N <- length(sts)
  par(mfrow = c(figure_dims(N)[1], figure_dims(N)[2]), mar = c(4,5,3,1), oma = c(0,0,3,0))
  for (st in sts){
    plot_object(object_list[[st]]$data_object)
    mtext(st, line = 0.5, cex = 0.9)
  }
  variable_name <- attr(object_list, "variable_name")
  if (variable_name == "VALUE_WW"){
    basis_txt <- "wet weight basis"
  } else if (variable_name == "VALUE_DW"){
    basis_txt <- "dry weight basis"
  } else if (variable_name == "VALUE_FB"){
    basis_txt <- "fat basis"
  }
  if (is.null(title)){
    mtext(paste0
          (
            attr(object_list, "det"), ", ",
            attr(object_list, "sp"), ", ",
            attr(object_list, "tis"), ", ",
            basis_txt
          ), outer = TRUE, line = 0.5)
  }
}
# plot_all_stations(test)


calc_models_several_determ <- function(determ, sp, tis, variable_name, yrs, determ_group = "x", trace = FALSE, plot = FALSE){
  list_of_result_lists <- vector("list", length(determ))
  if (variable_name == "VALUE_WW"){
    basis_txt <- "ww"
  } else if (variable_name == "VALUE_DW"){
    basis_txt <- "dw"
  } else if (variable_name == "VALUE_FB"){
    basis_txt <- "fb"
  }
  filename <- paste0("Results/", determ_group, "_", sp, "_", tis, "_", basis_txt, "_", yrs[1], "-", yrs[2], ".csv")
  for (i in 1:length(determ)){
    det <- determ[i]
    result_list <- calc_models_all_stations(det, sp = sp, tis = tis, variable_name = variable_name, yrs = yrs, trace = trace)
    if (plot)
      plot_all_stations(result_list)
    list_of_result_lists[[i]] <- result_list
  }
  names(list_of_result_lists) <- determ
  list_of_result_lists
}


#
# Make a copy of a CSV file with local (Norwegian) format (decimal = ",", separator = ";")
#
make_localformat_csv <- function(fn_read, fn_write){
  file_read = file(fn_read, "r")
  file_write = file(fn_write, "w+")
  L <- 1
  while (L > 0){
    content <- readLines(file_read, n = 1)
    L <- length(content)
    if (L > 0){
      content <- gsub(",", ";", content, fixed = TRUE)
      content <- gsub(".", ",", content, fixed = TRUE)
      cat(content, file = file_write)
      cat("\n", file = file_write)
    }
  }
  close(file_read)
  close(file_write)
  invisible(NULL)
}


#
# calc_models_one_station2
#
# Differs from "calc_models_one_station" that this one takes "medians_one_station" as input, while
#   "calc_models_one_station" takes "medians_all_stations" as input
#   "calc_models_one_station2" takes "medians_one_station" as input
# The input data should have the variables MYEAR, Median and Over_LOQ
# Also,
#  "calc_models_one_station2" has options gam (= FALSE by default), log (works only if gam = TRUE, is TRUE by default)
#

calc_models_one_station2 <- function(medians_one_station, gam = FALSE, log = TRUE){
  # df_med_regr_20yr <- get_medians_for_regression(medians_all_stations, station = station, yrs = yrs)
  df_med_object <- medians_one_station %>% select_for_regr_a() %>% select_for_regr_b()
  if (!gam){
    modelresults <- calc_models(df_med_object)
  } else {
    modelresults <- calc_models_gam(df_med_object, log = log)
  }
  df_med_object <- add_model_fits(df_med_object, modelresults, gam = gam)
  # df1 <- data_for_excel(medians_all_stations, station=station)
  df2 <- statistics_for_excel(df_med_object, modelresults, gam = gam)
  list(statistics_for_file = df2, data_object = df_med_object, modelresults = modelresults)
}

#
# Plot data (medians)
#   input =  output from 'calc_models_one_station2 '
#
plot_models_one_station <- function(result_object, logdata = FALSE, logscale = TRUE, title = "",
                                    colors = c("red", "blue")){
  if (logdata){
    plot(log(Median)~MYEAR, result_object$data_object$df_med_st, type = "n", main = title)
    points(log(Median)~MYEAR, result_object$data_object$df_med_st[result_object$data_object$sel_ts,], pch = 21, bg = "blue3")
    points(log(Median)~MYEAR, result_object$data_object$df_med_st[!result_object$data_object$sel_ts,], pch = 21, bg = "indianred1")
    lines(Fit_lin~MYEAR, result_object$data_object$df_med_st, lty = "dashed", col = colors[1])
    lines(Fit_nonlin~MYEAR, result_object$data_object$df_med_st, lty = "dotted", col = colors[2])
  } else if (!logdata){
    if (!logscale){
      plot(Median~MYEAR, result_object$data_object$df_med_st, main = title)
    } else {
      plot(Median~MYEAR, result_object$data_object$df_med_st, main = title, log = "y")
    }
    points(Median~MYEAR, result_object$data_object$df_med_st[result_object$data_object$sel_ts,], pch = 21, bg = "blue3")
    points(Median~MYEAR, result_object$data_object$df_med_st[!result_object$data_object$sel_ts,], pch = 21, bg = "indianred1")
    lines(exp(Fit_lin)~MYEAR, result_object$data_object$df_med_st, lty = "dashed", col = colors[1])
    lines(exp(Fit_nonlin)~MYEAR, result_object$data_object$df_med_st, lty = "dotted", col = colors[2])
  }
}

plot_models_one_station_gg <- function(result_object, title = "", log = TRUE){
  df <- result_object$data_object$df_med_st
  df$Included_in_trend <- "Included"
  df$Included_in_trend[!result_object$data_object$sel_ts] <- "Not included"
  if (log){
    gg <- ggplot(result_object$data_object$df_med_st, aes(MYEAR, log(Median)))
  } else {  
    gg <- ggplot(result_object$data_object$df_med_st, aes(MYEAR, Median))
  }
  gg <- gg + 
    geom_point(aes(color = Over_LOQ)) + 
    # geom_smooth(se = FALSE) +
    geom_line(aes(y = Fit_lin), color = "darkgreen", size = 2) +
    geom_line(aes(y = Fit_nonlin), color = "darkred", size = 2) +
    geom_line(data = result_object$modelresults$mod_nonlin$yFit, aes(x = Year, y = LowLimit), color = "darkred", linetype = 2) +
    geom_line(data = result_object$modelresults$mod_nonlin$yFit, aes(x = Year, y = HighLimit), color = "darkred", linetype = 2) +
    # geom_line(data = result_object$modelresults$mod_gam$yFit, aes(x = Year, y = Estimate), color = "blue", size = 2) +
    ggtitle(title)
  gg
}

statistics_for_excel_empty <- function(df_med){
  X <- data.frame(matrix(NA, 1, 19))
  colnames(X) <- c("Year1", "Year2", "N", "Nplus", "Mean", "p_linear", "p_nonlinear", 
                   "AICc_lin", "AICc_nonlin", "Lin_slope", "Lin_yr1", "Lin_yr2", 
                   "Nonlin_yr1", "Nonlin_yr2", "Over_LOQ_yr2", "Model_used", "P_change", "Dir_change", "SD_last")
  if (nrow(df_med) >= 1)
    X$SD_last <- tail(df_med$SD, 1)
  X
}



model_from_leveldata <- function(i, varname, yrs, leveldata, rawdata, plotname = "", ggplot = FALSE, gam = FALSE){
  df_values <- subset(rawdata, 
                      PARAM %in% leveldata$PARAM[i] &
                        LATIN_NAME %in% leveldata$LATIN_NAME[i] &
                        TISSUE_NAME %in% leveldata$TISSUE_NAME[i] &
                        STATION_CODE %in% leveldata$STATION_CODE[i] &
                        MYEAR %in% yrs)
  # col <- colnames(df_values) %in% varname
  # colnames(df_values)[col] <- "Measurement"
  df_values$Measurement <- df_values[,varname]
  df_values <- df_values[!is.na(df_values$Measurement),]
  df_med <- df_values %>%
    group_by(MYEAR) %>%
    summarise(Median = median(Measurement, na.rm = TRUE), N = n(), Over_LOQ = sum(is.na(FLAG1)), SD = sd(Measurement, na.rm = TRUE))
  if (!leveldata$PARAM[i] %in% c("VDSI", "BAP3O")){      # only these two have median numbers <= 0
    log_transform <- TRUE
  } else {
    log_transform <- FALSE
  }
  result_object <- try(calc_models_one_station2(df_med, gam = gam, log = log_transform))
  # str(result_object, 1)
  if (class(result_object)[1] == "try-error"){
    result_stat <- statistics_for_excel_empty(df_med)
  } else {
    # Result
    result_stat <- result_object$statistics_for_file
    # Plot
    if (plotname != ""){
      if (plotname != "window"){
        fn <- paste0(plotname, "_", leveldata$PARAM[i], "_", substr(leveldata$LATIN_NAME[i], 1, 3), "_", substr(leveldata$TISSUE_NAME[i], 1, 2), "_",
                     leveldata$STATION_CODE[i], "_", varname, "_", head(yrs,1), "-", tail(yrs,1), ".png")
        png2(fn, width = 7, height = 5)
      }
      tit <- paste0(leveldata$PARAM[i], ", ", leveldata$LATIN_NAME[i], ", ", leveldata$TISSUE_NAME[i], ", ",
                    leveldata$STATION_CODE[i], ", ", varname, ", ", head(yrs,1), "-", tail(yrs,1))
      if (!ggplot){
        plot_models_one_station(result_object, title = tit, log = log_transform)
      } else {
        print(
          plot_models_one_station_gg(result_object, title = tit, log = log_transform)
        )
      }
      if (plotname != "window")
        dev.off()
    }
  }
  list(result_object = result_object,
       statistics = 
         cbind(
           data.frame(PARAM = leveldata$PARAM[i], 
                      LATIN_NAME = leveldata$LATIN_NAME[i], 
                      TISSUE_NAME = leveldata$TISSUE_NAME[i], 
                      STATION_CODE = leveldata$STATION_CODE[i], stringsAsFactors = FALSE),
           result_stat,
           N_data = sum(df_med$MYEAR %in% yrs),                # this is the total number of years with data, regardless of data are over LOQ
           log_transformed = log_transform
         )
  )
}

#
# As 'model_from_leveldata' but takes data from file of annual medians (df_med), not raw data
# Note: here gam = TRUE by default (in contrast to model_from_leveldata)
#
# Hard-coded variable names that must ne in the fike of medians: 'Over_LOQ', 'N', 'SD'
#

model_from_medians <- function(param, species, tissue, station, basis, yrs, data_medians, varname = "Value", plotname = "", ggplot = FALSE, gam = TRUE){
  df_med <- subset(data_medians, 
                   PARAM %in% param & 
                     LATIN_NAME %in% species &
                     TISSUE_NAME %in% tissue &
                     STATION_CODE %in% station &
                     Basis %in% basis &
                     MYEAR %in% yrs)
  # Change name of response variable to "Median"
  colnames(df_med) <- sub(varname, "Median", colnames(df_med))
  # If we want to excape from the  hard-coded variable names, put varname_n_over_loq as a function argument and do this:
  # colnames(df_med) <- sub(varname_n_over_loq, "Over_LOQ", colnames(df_med))
  if (!param %in% c("VDSI", "VDSI/Intersex", "Intersex", "BAP3O")){      # only these two have median numbers <= 0
    log_transform <- TRUE
  } else {
    log_transform <- FALSE
  }
  result_object <- try(calc_models_one_station2(df_med, gam = gam, log = log_transform))
  # str(result_object, 1)
  if (class(result_object)[1] == "try-error"){
    result_stat <- statistics_for_excel_empty(df_med)
  } else {
    # Result
    result_stat <- result_object$statistics_for_file
    # Plot
    if (plotname != ""){
      if (plotname != "window"){
        fn <- paste0(plotname, "_", param, "_", substr(species, 1, 3), "_", substr(tissue, 1, 2), "_",
                     station, "_", Basis, "_", head(yrs,1), "-", tail(yrs,1), ".png")
        png2(fn, width = 7, height = 5)
      }
      tit <- paste0(param, ", ", species, ", ", tissue, ", ",
                    station, ", ", Basis, ", ", head(yrs,1), "-", tail(yrs,1))
      if (!ggplot){
        plot_models_one_station(result_object, title = tit, log = log_transform)
      } else {
        print(
          plot_models_one_station_gg(result_object, title = tit, log = log_transform)
        )
      }
      if (plotname != "window")
        dev.off()
    }
  }
  list(result_object = result_object,
       statistics = 
         cbind(
           data.frame(PARAM = param, 
                      LATIN_NAME = species, 
                      TISSUE_NAME = tissue, 
                      STATION_CODE = station, 
                      Basis = basis, stringsAsFactors = FALSE),
           result_stat,
           N_data = sum(df_med$MYEAR %in% yrs),                # this is the total number of years with data, regardless of data are over LOQ
           log_transformed = log_transform
         )
  )
}


model_from_medians_TEST <- function(PARAM, LATIN_NAME, TISSUE_NAME, STATION_CODE, Basis, yrs, data_medians, varname = "Value", plotname = "", ggplot = FALSE, gam = FALSE){
  df_med <- subset(data_medians, 
                   PARAM %in% PARAM & 
                     LATIN_NAME %in% LATIN_NAME &
                     TISSUE_NAME %in% TISSUE_NAME &
                     STATION_CODE %in% STATION_CODE &
                     Basis %in% Basis &
                     MYEAR %in% yrs)
  # Change name of response variable to "Median"
  colnames(df_med) <- sub(varname, "Median", colnames(df_med))
  # If we want to excape from the  hard-coded variable names, put varname_n_over_loq as a function argument and do this:
  # colnames(df_med) <- sub(varname_n_over_loq, "Over_LOQ", colnames(df_med))
  if (!PARAM %in% c("VDSI", "BAP3O")){      # only these two have median numbers <= 0
    log_transform <- TRUE
  } else {
    log_transform <- FALSE
  }
  result_object <- try(calc_models_one_station2(df_med, gam = gam, log = log_transform))
  # str(result_object, 1)
  if (class(result_object)[1] == "try-error"){
    result_stat <- statistics_for_excel_empty(df_med)
  } else {
    # Result
    result_stat <- result_object$statistics_for_file
    # Plot
    if (plotname != ""){
      if (plotname != "window"){
        fn <- paste0(plotname, "_", PARAM, "_", substr(LATIN_NAME, 1, 3), "_", substr(TISSUE_NAME, 1, 2), "_",
                     STATION_CODE, "_", Basis, "_", head(yrs,1), "-", tail(yrs,1), ".png")
        png2(fn, width = 7, height = 5)
      }
      tit <- paste0(PARAM, ", ", LATIN_NAME, ", ", TISSUE_NAME, ", ",
                    STATION_CODE, ", ", Basis, ", ", head(yrs,1), "-", tail(yrs,1))
      if (!ggplot){
        plot_models_one_station(result_object, title = tit, log = log_transform)
      } else {
        print(
          plot_models_one_station_gg(result_object, title = tit, log = log_transform)
        )
      }
      if (plotname != "window")
        dev.off()
    }
  }
  list(result_object = result_object,
       statistics = 
         cbind(
           data.frame(PARAM = PARAM, 
                      LATIN_NAME = LATIN_NAME, 
                      TISSUE_NAME = TISSUE_NAME, 
                      STATION_CODE = STATION_CODE, 
                      Basis = Basis, stringsAsFactors = FALSE),
           result_stat,
           N_data = sum(df_med$MYEAR %in% yrs),                # this is the total number of years with data, regardless of data are over LOQ
           log_transformed = log_transform
         )
  )
}


model_from_medians_OLD <- function(param, species, tissue, station, basis, yrs, data_medians, varname = "Value", plotname = "", ggplot = FALSE, gam = FALSE){
  df_med <- subset(data_medians, 
                   PARAM %in% param & 
                     LATIN_NAME %in% species &
                     TISSUE_NAME %in% tissue &
                     STATION_CODE %in% station &
                     Basis %in% basis &
                     MYEAR %in% yrs)
  # If we want to excape from the  hard-coded variable names, put varname_n_over_loq as a function argument and do this:
  # colnames(df_med) <- sub(varname_n_over_loq, "Over_LOQ", colnames(df_med))
  if (!leveldata$PARAM[i] %in% c("VDSI", "BAP3O")){      # only these two have median numbers <= 0
    log_transform <- TRUE
  } else {
    log_transform <- FALSE
  }
  result_object <- try(calc_models_one_station2(df_med, gam = gam, log = log_transform))
  # str(result_object, 1)
  if (class(result_object)[1] == "try-error"){
    result_stat <- statistics_for_excel_empty(df_med)
  } else {
    # Result
    result_stat <- result_object$statistics_for_file
    # Plot
    if (plotname != ""){
      if (plotname != "window"){
        fn <- paste0(plotname, "_", leveldata$PARAM[i], "_", substr(leveldata$LATIN_NAME[i], 1, 3), "_", substr(leveldata$TISSUE_NAME[i], 1, 2), "_",
                     leveldata$STATION_CODE[i], "_", varname, "_", head(yrs,1), "-", tail(yrs,1), ".png")
        png2(fn, width = 7, height = 5)
      }
      tit <- paste0(leveldata$PARAM[i], ", ", leveldata$LATIN_NAME[i], ", ", leveldata$TISSUE_NAME[i], ", ",
                    leveldata$STATION_CODE[i], ", ", varname, ", ", head(yrs,1), "-", tail(yrs,1))
      if (!ggplot){
        plot_models_one_station(result_object, title = tit, log = log_transform)
      } else {
        print(
          plot_models_one_station_gg(result_object, title = tit, log = log_transform)
        )
      }
      if (plotname != "window")
        dev.off()
    }
  }
  list(result_object = result_object,
       statistics = 
         cbind(
           data.frame(PARAM = leveldata$PARAM[i], 
                      LATIN_NAME = leveldata$LATIN_NAME[i], 
                      TISSUE_NAME = leveldata$TISSUE_NAME[i], 
                      STATION_CODE = leveldata$STATION_CODE[i], stringsAsFactors = FALSE),
           result_stat,
           N_data = sum(df_med$MYEAR %in% yrs),                # this is the total number of years with data, regardless of data are over LOQ
           log_transformed = log_transform
         )
  )
}


# Function for getting the files with the newest date stamps
# Finds the files fitting 'filepattern'
# Then extract time between 'pattern_before_time' and 'pattern_after_time'
get_newest_filenames <- function(filepattern,
                                 pattern_before_time,
                                 pattern_after_time,
                                 folder = "Data", 
                                 tz = "CEST"       # time zone - only affects 'time passed'
){
  fns <- dir(folder, filepattern)
  # timestamps <- stringr::str_match(fns, "back_(.+).rda")[,2]
  search_pattern <- paste0(pattern_before_time, "(.+)", pattern_after_time)
  # timestamps <- stringr::str_match(fns, "back_(.+).rda")[,2]
  timestamps <- stringr::str_match(fns, search_pattern)[,2]
  latest_time <- max(ymd_hms(timestamps, tz = tz), na.rm = TRUE)
  cat("Latest time: ")
  print(latest_time)
  cat("How long time since file was written? ")
  print(Sys.time() - latest_time)
  sel <- ymd_hms(timestamps, tz = tz) %in% latest_time
  fns[sel]
}

