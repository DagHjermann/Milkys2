
#
# This script is also used by App01
#

#
# Get series number from PARAM + STATION_CODE (and more if necessary)
#
get_data <- function(param, stationcode, 
                     tissue = NULL,
                     species = NULL,
                     basis = NULL,
                     data){
  
  # browser()
  
  sel <- with(data, PARAM %in% param & STATION_CODE %in% stationcode)
  
  if (!is.null(tissue))
    sel <- sel & with(data, TISSUE_NAME %in% tissue)
  
  if (!is.null(species))
    sel <- sel & with(data, LATIN_NAME %in% species)
  
  if (!is.null(basis))
    sel <- sel & with(data, Basis %in% basis)
  
  if (sum(sel) == 0){
    stop("No time series found")
  }
  
  if (sum(sel) > 1){
    cat("Values of TISSUE_NAME found:", unique(data$TISSUE_NAME), "\n")
    cat("Values of LATIN_NAME found:", unique(data$LATIN_NAME), "\n")
    warning(sum(sel), " time series found in data. You may want to specify 'tissue' and/or 'species'.")
  }
  
  data[sel,]
  
}


#
# Extract and plot data from results (on files) and data (in memory)
#
# This function is built on 'tsplot_param' from script 125..functions, but has been adapted
# It just finds the value of 'seriesno', then lets 'plot_timeseries_seriesno' do the actual plotting
#
plot_timeseries <- function(param, stationcode,
                            tissue = NULL,
                            species = NULL,
                            basis = NULL,
                            y_scale = "ordinary",
                            ymax_perc = 100,
                            xmin_rel = 0, xmax_rel = 0,
                            eqs = TRUE, proref = 1,
                            folder,
                            data = dat_all_prep3, 
                            data_series = dat_series_trend,
                            data_trend = NULL,
                            quantiles = c(0.25, 0.75),
                            medians = TRUE,
                            allsamples = FALSE){
  
  browser()
  
  seriesno <- get_seriesno(
    param = param, 
    stationcode = stationcode,
    tissue = tissue,
    species = species,
    basis = basis,
    data_series = data_series)
  
  if (length(seriesno) > 1){
    rawdata <- extract_raw_data(seriesno, data = data, data_series = data_series)
    txt1 <- paste("Species found:", paste(unique(rawdata$LATIN_NAME), collapse = ", "), "\n")    
    txt2 <- paste("Tissues found:", paste(unique(rawdata$TISSUE_NAME), collapse = ", "), "\n") 
    stop(">1 series selected. You must probably add 'tissue = ...' (see below for tissues at this station) \n\n",
         txt1, txt2)
  }
  
  gg <- plot_timeseries_seriesno(seriesno,
                                 y_scale = y_scale,
                                 ymax_perc = ymax_perc,
                                 xmin_rel = xmin_rel, xmax_rel = xmax_rel,
                                 eqs = eqs, proref = proref,
                                 folder = folder,
                                 data = data, 
                                 data_series = data_series,
                                 data_trend = data_trend,
                                 quantiles = quantiles,
                                 medians = medians,
                                 allsamples = allsamples)
  
  gg
  
}

if (FALSE){
  
  # Before testing, run/source this script to load all data
  source("App01_timeseries/app01_test.R")
  
  # In app:
  folder_results <- "../Data/125_results_2021_07"
  # For testing:
  folder_results <- "Data/125_results_2021_07"
  
  debugonce(plot_timeseries)
  debugonce(plot_timeseries_seriesno)
  plot_timeseries("CB153", "11X", folder = folder_results, proref = "1")
  plot_timeseries("CB153", "19N", tissue = "Blod", folder = folder_results, proref = "1")
  
  plot_timeseries("CB138", "10A2", folder = folder_results, proref = "", 
                  quantiles = c(0,1))
  debugonce(plot_timeseries_seriesno)
  plot_timeseries("CB138", "10A2", folder = folder_results, proref = "", 
                  quantiles = c(0,1), allsamples = TRUE, y_scale = "ordinary")
  plot_timeseries("CB138", "10A2", folder = folder_results, proref = "", 
                  quantiles = c(0,1), allsamples = TRUE, y_scale = "ordinary", medians = FALSE)
  
}


#
# Extract and plot data from results (on files) and data (in memory)
#
plot_timeseries_seriesno <- function(seriesno,
                                     y_scale = "ordinary",
                                     ymax_perc = 100,
                                     xmin_rel = 0, xmax_rel = 0,
                                     eqs = TRUE, proref = 1,
                                     folder,
                                     data = dat_all_prep3, 
                                     data_series = dat_series_trend,
                                     data_trend = NULL,
                                     quantiles = c(0.25, 0.75),
                                     medians = TRUE,
                                     allsamples = FALSE,
                                     trendtext_size = 4){
  
  # browser()
  
  # fn <- sprintf("trend_%04.0f.rda", seriesno)
  # resultlist <- readRDS(paste0(folder, "/", fn))
  resultlist
  
  # str(resultlist, 1)
  
  # debugonce(get_pointdata)
  df_points <- extract_raw_data(seriesno, data = data, data_series = data_series) %>%
    mutate(y_comb = ifelse(is.na(y), threshold, y))
  
  # EQS and Proref
  include_eqs <- !is.na(df_points$EQS[1]) & eqs
  
  proref_x <- as.numeric(strsplit(proref, split = ",")[[1]])
  include_proref <- !is.na(df_points$Proref[1]) & length(proref_x) > 0
  
  # Get unit to print on y axis  
  unit_print <- get_unit_text(tail(df_points$UNIT, 1), tail(df_points$Basis, 1), tail(df_points$PARAM, 1))
  
  df_median <- df_points %>%
    group_by(x) %>%
    summarise(
      y = median(y_comb, na.rm = TRUE), 
      n = n(), 
      n_overLOQ = sum(is.na(threshold)),
      ymin = quantile(y_comb, probs = quantiles[1]),
      ymax = quantile(y_comb, probs = quantiles[2]),
      .groups = "drop") %>%
    mutate(overLOQ = n_overLOQ > (0.5*n))
  
  # GAM trend analysis
  mod <- mgcv::gam(y ~ s(x), data = df_median)
  plot_data <- data.frame(x = seq(min(df_median$x), max(df_median$x), length = 50))
  pred <- mgcv::predict.gam(mod, plot_data, se.fit = TRUE)
  plot_data$y <- pred$fit
  plot_data$y_q2.5 <- pred$fit + qt(0.025, mod$df.residual)*pred$se.fit
  plot_data$y_q97.5 <- pred$fit + qt(0.975, mod$df.residual)*pred$se.fit
  resultlist$plot_data[[1]] <- plot_data
  
  titlestring <- paste0(df_points$Param_name[1], " in ", df_points$Species_name[1], " at ", df_points$Station_name[1])
  subtitlestring <- paste0("Station code: ", resultlist$STATION_CODE, " (region: ", df_points$Region[1], "). ", 
                           str_to_sentence(df_points$Tissue_name[1]), " (basis ", resultlist$Basis, "), ", resultlist$LATIN_NAME)
  
  if (y_scale %in% c("ordinary", "log scale")){
    df_median <- df_median %>% 
      mutate(
        y = exp(y),
        ymin = exp(ymin),
        ymax = exp(ymax))
    df_points <- df_points %>% 
      mutate(y = exp(y),
             threshold = exp(threshold))
  }
  
  # Set x limits
  x_limits <- range(df_points$x) + c(xmin_rel, xmax_rel)
  
  # Set y limits
  rn <- c(min(df_median$ymin), max(df_median$ymax))
  y_limits <- c(rn[1], rn[1] + (rn[2]-rn[1])*ymax_perc/100)
  if (include_eqs){
    y_limits[1] <- min(rn[1], df_points$EQS[1])
  }
  
  # Start plot (no layer actually being plotted yet)
  gg <- ggplot(df_median, aes(x, y))
  
  # If there are results from trend analysis, add the ttend (ribbon + line) 
  if (!is.null(resultlist$plot_data)){
    check_bug <- which(resultlist$k_values == resultlist$k_sel) == resultlist$k_sel
    # k_sel <- resultlist$k_sel               # old version - gives error, or (worse) picks the wrong model
    # k_sel <- as.character(resultlist$k_sel)   # corrected version
    k_sel <- 1
    if (y_scale %in% c("ordinary", "log scale")){
      # resultlist$plot_data[[k_sel]] <- resultlist$plot_data[[k_sel]] %>% 
      #   mutate(
      #     y = exp(y),
      #     y_q2.5 = exp(y_q2.5),
      #     y_q97.5 = exp(y_q97.5))
      resultlist$plot_data[[k_sel]] <- resultlist$plot_data[[k_sel]] %>% 
        mutate(
          y = exp(y),
          y_q2.5 = exp(y_q2.5),
          y_q97.5 = exp(y_q97.5))
    }
    gg <- gg +
      geom_ribbon(data = resultlist$plot_data[[k_sel]], aes(ymin = y_q2.5, ymax = y_q97.5), fill = "grey70") +
      geom_line(data = resultlist$plot_data[[k_sel]])
  }
  
  # Checks whether old version of k_sel might have differed from the new version - see "old version" 14 lines up  
  # If k_sel was higher than thelength of k_values, then it gave an error before, so no need to warn
  # if (!check_bug & resultlist$k_sel <= length(resultlist$k_values)){
  #   warning("Previous version of this plot (before 28.10.2022) probably showed the wrong model")
  # }
  
  # If allsamples = TRUE, add points for the individual samples to the plot 
  if (allsamples){
    gg <- gg +
      geom_point(data = df_points %>% filter(!is.na(y))) +
      geom_point(data = df_points %>% filter(!is.na(threshold)), aes(y = threshold), shape = 6)
  }
  # Add medians (points) and the quantiles (vertical lines) 
  if (quantiles[1] == 0 & quantiles[2] == 1){
    range_txt <- "The range of the vertical bars is the min/max of all samples"
  } else {
    range_txt <- paste0("The range of the vertical bars is the ", 
                        quantiles[1]*100, "% - ", quantiles[2]*100, "%",
                        " percentiles of the samples")
  }
  # Add medians (points) and the quantiles (vertical lines) 
  if (medians){
    gg <- gg +
      geom_point(data = df_median %>% filter(overLOQ), shape = 21, fill = "red2", size = rel(3)) +
      geom_point(data = df_median %>% filter(!overLOQ), shape = 25, fill = "red2", size = rel(3)) +
      geom_linerange(data = df_median, aes(ymin = ymin, ymax = ymax), color = "red2")
  }
  gg <- gg +
    labs(title = titlestring, subtitle = subtitlestring, caption = range_txt) +
    theme_bw()
  
  # Make "trend text" to add in the top right corner  
  if (!is.null(data_trend)){
    df_trend_sel <- data_trend %>% 
      filter(PARAM %in% resultlist$PARAM,
             STATION_CODE %in% resultlist$STATION_CODE,
             TISSUE_NAME %in% resultlist$TISSUE_NAME,
             LATIN_NAME %in% resultlist$LATIN_NAME,
             Basis  %in% resultlist$Basis) %>%
      as.data.frame()
    trendstring <- map_chr(c("long","short"), get_trendstring, trenddata = df_trend_sel)
    trendstring_comb <- paste0(                  # paste0(trendstring[1], "\n", trendstring[2])
      "Long-term: ", trendstring[1], "\n",
      "Short-term: ", trendstring[2])
    #    "Long-term: ", subset(df_trend_sel, Trend_type == "long")$Trend_string, "\n",
    #   "Short-term: ", subset(df_trend_sel, Trend_type == "short")$Trend_string)
    
    # Optionally: Put the whole plot on log scale   
    if (y_scale == "log scale"){
      gg <- gg +
        scale_y_log10()
    }
    
    # Optionally: Add a line for EQS     
    if (include_eqs){
      gg <- gg +
        geom_hline(yintercept = df_points$EQS[1], color = "red2", linetype = "dashed", size = rel(1.5)) +
        annotate("text", x = x_limits[1], y = df_points$EQS[1], label = "EQS", 
                 hjust = 0.5, vjust = -1, 
                 size = 5, color = "red2")
    }
    
    # Optionally: Add one or several lines (e.g. 1x, 2x, 5x...) for Proref      
    if (include_proref){
      proref_vals <- df_points$Proref[1]*proref_x
      if (identical(proref_x,1)){
        proref_text <- "PROREF"
      } else {
        proref_text <- paste0(proref_x, "x PROREF")
      }
      gg <- gg +
        geom_hline(yintercept = proref_vals, color = "blue2", linetype = "dotted", 
                   size = rel(1)) +
        annotate("text", x = x_limits[1], y = proref_vals, label = proref_text, 
                 hjust = 0.25, vjust = -1, 
                 size = 4, color = "blue2")
    }
    
    # Add "trend text" (made above) in the top right corner  
    trendstring_x <- x_limits[2] + 0.02*diff(x_limits)
    gg <- gg +
      coord_cartesian(
        xlim = x_limits, ylim = y_limits) +
      labs(
        y = unit_print,  #y = paste0("Concentration, ", unit_print),
        x = "") +
      annotate("text", x = trendstring_x, y = Inf, label = trendstring_comb, hjust = 1, vjust = 1.2, size = trendtext_size, colour = "blue3")
  }
  
  # Return the ggplot object
  gg
  
}

#
# GAM trend analysis
#
# Output: data frame with y, y_q2.5, y_q97.5 (estimate and 95% range) 
# Names of output: x and y have the original names of the input
#   the 95% range have original 'y' name plus "_lo" and "_hi"
#

get_gam_data <- function(data, x = "x", y = "y", res = 0.25){
  names(data)[which(names(data) == x)[1]] <- "x"
  names(data)[which(names(data) == y)[1]] <- "y"
  mod <- mgcv::gam(y ~ s(x), data = data)
  result <- data.frame(x = seq(min(data$x), max(data$x), by = res))
  pred <- mgcv::predict.gam(mod, result, se.fit = TRUE)
  result$y <- pred$fit
  result$se <- pred$se.fit
  result$y_q2.5 <- pred$fit + qt(0.025, mod$df.residual)*pred$se.fit
  result$y_q97.5 <- pred$fit + qt(0.975, mod$df.residual)*pred$se.fit
  names(result)[1:5] <- c(x, y, "se", paste0(y, "_lo"), paste0(y, "_hi"))
  result
}



get_change_from_gamdata <- function(gamdata, x, y, dx = NULL){
  if (is.null(dx)){
    y1 <- gamdata[[y]][1]
    y1_se <- gamdata$se[1]
    dx <- max(gamdata[[x]]) - min(gamdata[[x]])
  } else {
    sel_x <- max(gamdata[[x]]) - dx
    sel_i <- which(gamdata[[x]] %in% sel_x)
    y1 <- gamdata[[y]][sel_i]
    y1_se <- gamdata$se[sel_i]
  }
  y2 <- tail(gamdata[[y]], 1)
  y2_se <- tail(gamdata$se, 1)
  y_diff <- y2 - y1
  y_diff_se <- sqrt(y1_se^2 + y2_se^2)
  t <- abs(y_diff/y_diff_se)
  data.frame(
    change = y_diff,
    dx = dx,
    change_per_x = y_diff/dx, 
    t = t,
    p = 2*(1-pnorm(t))
  )
}

if (FALSE){
  
  # Calculate annual change in percent from change in log(y)
  # y2 = y1*g
  # y3 = y1*g*g = y1*g^2
  # y10 = y1*g^9
  # log(y10) = log(y1) + log(g^9)
  #          = log(y1) + 9*log(g)
  # log(g) = (log(y10) - log(y1))/9
  # g = exp((log(y10) - log(y1))/9)
  # percent annual change = 100*(g-1)
  
  y <- c(100, rep(NA,9))
  for (i in 2:10){
    y[i] <- y[i-1]*1.05
  }
  y
  (log_change <- log(y[10]) - log(y[1]))
  (g <- exp(log_change/9))
  (perc_annual_change = 100*(-1))

}


#
# Get trend string, with percent pro anno increase/decrease
# NOTE: assuming that change_data are from analysis of 
#
get_trendstring <- function(change_data){
  result <- change_data %>%
    mutate(
      perc_change = 100*(exp(change_per_x)-1),
      text1 = case_when(
        change > 0 & p <= 0.05 ~ "Increasing",
        change < 0 & p <= 0.05 ~ "Decreasing",
        p > 0.05 ~ "No change"),
      text2 = case_when(
        p <= 0.05 ~ paste0(text1, " (", sprintf("%+.1f", perc_change), "% annually)"),
        p > 0.05 ~ text1)
    )
  pull(result, text2)
}

get_trendstring_comb <- function(gamdata, x = "x", y = "y"){
  change_all <- get_change_from_gamdata(gamdata, x, y)
    if (change_all$dx > 10){
      change_10yr <- get_change_from_gamdata(gamdata, x, y, dx = 10)
      result <- paste(
        "Entire time series:", get_trendstring(change_all), "\n",
        "Last 10 years:", get_trendstring(change_10yr))
    } else {
      result <- get_trendstring(change_all)
  } 
  result
}
  

if (FALSE){
  
  # Testing 'get_gam_data' and 'get_change_from_gamdata'  
  
  test <- data.frame(year = 1:20)
  test$value <- 4 + 0.15*test$year - 0.006*test$year^2 + rnorm(20, sd = 0.1)
  test$value <- 4 + 0.15*test$year - 0.0067*test$year^2 + rnorm(20, sd = 0.1)
  test$value <- 4 + 0.15*test$year - 0.007*test$year^2 + rnorm(20, sd = 0.1)
  # test$value <- 4 + 0.15*test$year - 0.008*test$year^2 + rnorm(20, sd = 0.1)
  plot(value~year, data = test)
  # debugonce(get_gam_data)
  gamdata <- get_gam_data(test, x = "year", y = "value")
  head(gamdata)
  lines(value~year, data = gamdata, type = "l")  
  lines(value_lo~year, data = gamdata, type = "l", lty = "dashed")  
  lines(value_hi~year, data = gamdata, type = "l", lty = "dashed")  
  
  # debugonce(get_change_from_gamdata)
  (change1 <- get_change_from_gamdata(gamdata, "year", "value"))
  (change2 <- get_change_from_gamdata(gamdata, "year", "value", dx = 10))
  get_trendstring(change1)
  get_trendstring(change2)
  get_trendstring_comb(gamdata, "year", "value")
  
}


get_median_data <- function(data, quantiles = c(0.25,0.75)){
  data %>%
    group_by(x) %>%
    summarise(
      y = median(y, na.rm = TRUE), 
      n = n(), 
      n_overLOQ = sum(is.na(FLAG1)),
      ymin = quantile(y, probs = quantiles[1]),
      ymax = quantile(y, probs = quantiles[2]),
      .groups = "drop") %>%
    mutate(overLOQ = n_overLOQ > (0.5*n))
}


get_eqs <- function(param, latin_name, basis, eqsdata){
  eqs <- eqsdata %>%
    filter(PARAM %in% param,
           is.na(LATIN_NAME) | LATIN_NAME %in% latin_name,
           Basis == "WW") %>%
    pull(EQS)
  if (length(eqs) > 1){
    warning("More than one EQS found")
  }
  eqs[1]
}

get_proref <- function(param, latin_name, tissue_name = NULL, basis, prorefdata){
  if (is.null(tissue_name) & latin_name %in% "Mytilus edulis"){
    tissue_name <- "Whole soft body"
  }
  proref <- prorefdata %>%
    filter(PARAM %in% param, LATIN_NAME %in% latin_name,
           TISSUE_NAME %in% tissue_name, Basis == basis) %>%
    pull(Proref)
  if (length(proref) > 1){
    warning("More than one proref found")
  }
  proref[1]
}

if (FALSE){
  get_proref("HG", "Mytilus edulis", "Whole soft body", basis = "WW", prorefdata = lookup_proref)
}



#
# Extract and plot data from results (on files) and data (in memory)
#
# Based on 'plot_timeseries_seriesno' but the goal is that the function is fed with
# - medians and/or raw data  
# - raw data for showing trend (x values and corrsponding values for y plus lower and upper bound of y)

# All data sets must have columns 'x' and 'y'
# data_medians and data_trend must have columns 'ymin' and 'ymax'
# data_medians must have columns 'overLOQ'
# data_raq must have columns 'loq'

if (FALSE){
  plot_timeseries_trend()
}

plot_timeseries_trend <- function(data_medians = NULL,
                                  data_raw = NULL,
                                  data_trend = NULL,
                                  y_scale = "ordinary",
                                  ymax_perc = 100,
                                  xmin_rel = 0, xmax_rel = 0,
                                  allsamples = FALSE,
                                  eqs = FALSE, 
                                  proref = "1",
                                  value_eqs = NA, 
                                  value_proref = NA,
                                  quantiles = c(0.25, 0.75),
                                  y_label = "Y", x_label = "",
                                  titlestring = NULL, 
                                  subtitlestring = NULL,
                                  trendtext = "",
                                  trendtext_size = 4){
  
  # browser()
  
  # fn <- sprintf("trend_%04.0f.rda", seriesno)
  # resultlist <- readRDS(paste0(folder, "/", fn))

  # str(resultlist, 1)
  
  # EQS and Proref
  include_eqs <- !is.na(value_eqs) & eqs
  
  proref_x <- as.numeric(strsplit(proref, split = ",")[[1]])
  include_proref <- !is.na(value_proref) & length(proref_x) > 0
  
  # Get unit to print on y axis  
  
  # unit_print <- get_unit_text(tail(data$UNIT, 1), tail(data$BASIS, 1), tail(data$PARAM, 1))

  if (y_scale %in% c("ordinary", "log scale")){
    data_medians <- data_medians %>% 
      mutate(
        y = exp(y),
        ymin = exp(ymin),
        ymax = exp(ymax))
    data_raw <- data_raw %>% 
      mutate(y = exp(y),
             LOQ = exp(LOQ))
  }
  
  # Set x limits
  x_limits <- range(data_raw$x) + c(xmin_rel, xmax_rel)
  
  # Set y limits
  rn <- c(min(data_medians$ymin), max(data_medians$ymax))
  y_limits <- c(rn[1], rn[1] + (rn[2]-rn[1])*ymax_perc/100)
  if (include_eqs){
    y_limits[1] <- min(rn[1], data_raw$EQS[1])
  }
  
  # Start plot (no layer actually being plotted yet)
  gg <- ggplot(data_medians, aes(x, y))
  
  # If there are results from trend analysis, add the ttend (ribbon + line) 
  if (!is.null(data_trend)){
    if (y_scale %in% c("ordinary", "log scale")){
      data_trend <- data_trend %>% 
        mutate(
          y = exp(y),
          ymin = exp(ymin),
          ymax = exp(ymax))
    }
    gg <- gg +
      geom_ribbon(data = data_trend, aes(ymin = ymin, ymax = ymax), fill = "grey70") +
      geom_line(data = data_trend)
  }
  
  # If allsamples = TRUE, add points for the individual samples to the plot 
  if (allsamples){
    gg <- gg +
      geom_point(data = data_raw %>% filter(!is.na(y))) +
      geom_point(data = data_raw %>% filter(!is.na(LOQ)), aes(y = LOQ), shape = 6)
  }
  # Add medians (points) and the quantiles (vertical lines) 
  if (quantiles[1] == 0 & quantiles[2] == 1){
    range_txt <- "The range of the vertical bars is the min/max of all samples"
  } else {
    range_txt <- paste0("The range of the vertical bars is the ", 
                        quantiles[1]*100, "% - ", quantiles[2]*100, "%",
                        " percentiles of the samples")
  }
  # Add medians (points) and the quantiles (vertical lines) 
  if (!is.null(data_medians)){
    gg <- gg +
      geom_point(data = data_medians %>% filter(overLOQ), shape = 21, fill = "red2", size = rel(3)) +
      geom_point(data = data_medians %>% filter(!overLOQ), shape = 25, fill = "red2", size = rel(3)) +
      geom_linerange(data = data_medians, aes(ymin = ymin, ymax = ymax), color = "red2")
  }
  if (!is.null(titlestring))
    gg <- gg + labs(title = titlestring)
  if (!is.null(subtitlestring))
    gg <- gg + labs(subtitle = subtitlestring)
  
  gg <- gg + 
    labs(caption = range_txt) +
    theme_bw()
  
  
  # Optionally: Put the whole plot on log scale   
  if (y_scale == "log scale"){
    gg <- gg +
      scale_y_log10()
  }
  
  # Optionally: Add a line for EQS     
  if (include_eqs){
    gg <- gg +
      geom_hline(yintercept = value_eqs, color = "red2", linetype = "dashed", size = rel(1.5)) +
      annotate("text", x = x_limits[1], y = value_eqs, label = "EQS", 
               hjust = 0.5, vjust = -1, 
               size = 5, color = "red2")
  }
  
  # Optionally: Add one or several lines (e.g. 1x, 2x, 5x...) for Proref      
  if (include_proref){
    proref_vals <- value_proref*proref_x
    if (identical(proref_x,1)){
      proref_text <- "PROREF"
    } else {
      proref_text <- paste0(proref_x, "x PROREF")
    }
    gg <- gg +
      geom_hline(yintercept = proref_vals, color = "blue2", linetype = "dotted", 
                 size = rel(1)) +
      annotate("text", x = x_limits[1], y = proref_vals, label = proref_text, 
               hjust = 0.25, vjust = -1, 
               size = 4, color = "blue2")
  }
  
  # Add "trend text" (made above) in the top right corner  
  trendtext_x <- x_limits[2] + 0.02*diff(x_limits)
  gg <- gg +
    coord_cartesian(
      xlim = x_limits, ylim = y_limits) +
    labs(
      y = y_label,  
      x = x_label) +
    annotate("text", x = trendtext_x, y = Inf, label = trendtext, hjust = 1, vjust = 1.2, size = trendtext_size, colour = "blue3")

# Return the ggplot object
gg

}



#
# Get trend string, with percent pro anno increase/decrease
#
get_trendtext <- function(x, trenddata){
  trenddata_selected <- trenddata[trenddata$Trend_type %in% x,]
  if (nrow(trenddata_selected) > 1)
    warning(">1 row in trend results selected")
  txt <- trenddata_selected$Trend_string[1]
  perc_change <- trenddata_selected$Perc_annual[1]
  ifelse(
    txt %in% c("Increasing", "Decreasing"),
    paste0(txt, " (", sprintf("%+.1f", perc_change), "% annually)"),
    txt
  )
}

if (FALSE){
  test <- df_trend %>% filter(PARAM %in% "HG" & STATION_CODE == "30B" & Basis == "WWa")
  # debugonce(get_trendtext)
  get_trendtext("long", test)
  get_trendtext("short", test)
  map_chr(c("long", "short"), get_trendtext, trenddata = test)
}


#
# Given unit, basis and param, make a suitable "unit text" for y axis label
#
get_unit_text <- function(unit, basis, param){
  # Unit
  if (param %in% "VDSI"){                                  # special treatment of VDSI
    unit_print <- "VDSI"                                    #endret fra "VDS average"
  } else if (basis %in% "WW" & unit %in% "MG_P_KG"){       # if it is something else than VDSI, we check basis and unit
    unit_print <- "Concentration, mg/kg (w.w.)"
  } else if (basis %in% "WW" & unit %in% "UG_P_KG"){
    unit_print <- expression(Concentration*","*~mu*g/kg~(w.w.))
  } else if (basis %in% "DW" & unit %in% "MG_P_KG"){
    unit_print <- "Concentration, mg/kg (d.w.)"
  } else if (basis %in% "DW" & unit %in% "UG_P_KG"){
    unit_print <- expression(Concentration,~mu*g/kg~(d.w.))
  } else if (basis %in% "FB" & unit %in% "MG_P_KG"){
    unit_print <- "Concentration, mg/kg (lipid basis)"
  } else if (basis %in% "FB" & unit %in% "UG_P_KG"){
    unit_print <- expression(Concentration*","*~mu*g/kg~(lipid~basis))
  } else if (basis %in% "WWa" & unit %in% "MG_P_KG"){
    unit_print <- "Concentration, mg/kg (w.w.) for 50 cm cod"
  } else if (basis %in% "WWa" & unit %in% "UG_P_KG"){
    unit_print <- expression(Concentration*","*~mu*g/kg~(w.w.)~plain("for 50 cm cod"))
  } else if (basis %in% "DWa" & unit %in% "MG_P_KG"){
    unit_print <- "Concentration, mg/kg (d.w.) for 50 cm cod"
  } else if (basis %in% "DWa" & unit %in% "UG_P_KG"){
    unit_print <- expression(Concentration*","*~mu*g/kg~(d.w.)~plain("for 50 cm cod"))
  } else if (basis %in% "FBa" & unit %in% "MG_P_KG"){
    unit_print <- "Concentration, mg/kg (lipid basis) for 50 cm cod"
  } else if (basis %in% "FBa" & unit %in% "UG_P_KG"){
    unit_print <- expression(Concentration*","*~mu*g/kg~(lipid~basis)~plain("for 50 cm cod"))
  } else {
    unit_print <- unit
  }
  unit_print
}


save_plot <- function(ggplot, fn, folder, suffix = "", windows = FALSE,
                      width = 5.6, height = 4.4,                                  # set plot size, in inches (but see note in code for Linux)
                      dpi_windows = 450, dpi_linux = 400                          # set plot resolution (dots per inch))
){
  # fn <- paste0(folder, "/", plot_single_result$fn)
  # If you want, you can add a suffix to the file name (e.g. "_2") by 
  #   setting e.g. suffix = "_2"  (note: works only on png and jpg!)
  fn <- sub(".png", paste0(suffix, ".png"), fn)
  fn <- sub(".jpg", paste0(suffix, ".jpg"), fn)
  if (windows){
    # If we are in Windows, we use ggsave() 
    ggsave(fn, plot_single_result$gg, height = height, width = width, dpi = dpi_windows)
  } else {
    # If we are in Linux (e.g. Jupyterhub), we use 'good old' png()   
    # Note that we also have to increase the size of the plot a bit (in inches),
    #   otherwise the trend symbols look bad
    # Because of that size increase, we also have to increase the size of 
    #   numbers and labels first
    ggplot <- ggplot +
      theme(axis.text = element_text(size = 11),     # set size of numbers along axes
            axis.title = element_text(size = 12),    # set size of axis labels
            plot.title = element_text(size = 13))    # set size of plot title
    #svglite(fn, height = height*1.2, width = width*1.2)                              ELU:testing with svglite (not png)
    png(fn, height = height*1.2, width = width*1.2, units = "in", res = dpi_linux)    # create an empty plot file 
    print(ggplot)                                                                         # ...plot on it...
    dev.off()                                                                         # ...and save it
  }
  invisible(fn)   # returns file name invisibly
}


