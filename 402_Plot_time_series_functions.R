
#
# This script is also used by App01
#

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
  
  # browser()
  
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
  
  fn <- sprintf("trend_%04.0f.rda", seriesno)
  resultlist <- readRDS(paste0(folder, "/", fn))
  
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
  
  titlestring <- paste0(df_points$Param_name[1], " in ", df_points$Species_name[1], " at ", df_points$Station_name[1])
  subtitlestring <- paste0("Station code: ", resultlist$STATION_CODE, " (region: ", df_points$Region[1], "). ", 
                           str_to_sentence(df_points$Tissue_name[1]), " (basis ", resultlist$Basis, "), ", resultlist$LATIN_NAME)
  
  # Some parameters (VDSI, sums with exloq, etc.) contain zeros and we used log(x+1) instead of log(x) fpr their analysis
  # See 125_extra1_2021.R
  log_xplus_1 <- grepl("VDSI", df_points$PARAM[1]) | grepl("_exloq", df_points$PARAM[1], fixed = TRUE)

  if (y_scale %in% c("ordinary", "log scale") & !log_xplus_1){
    df_median <- df_median %>% 
      mutate(
        y = exp(y),
        ymin = exp(ymin),
        ymax = exp(ymax))
    df_points <- df_points %>% 
      mutate(y = exp(y),
             threshold = exp(threshold))
  } else if (y_scale %in% c("ordinary", "log scale") & log_xplus_1){
      df_median <- df_median %>% 
        mutate(
          y = exp(y)-1,
          ymin = exp(ymin)-1,
          ymax = exp(ymax)-1)
      df_points <- df_points %>% 
        mutate(y = exp(y)-1,
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
    k_sel <- as.character(resultlist$k_sel)   # corrected version
    if (y_scale %in% c("ordinary", "log scale") & !log_xplus_1){
      resultlist$plot_data[[k_sel]] <- resultlist$plot_data[[k_sel]] %>% 
        mutate(
          y = exp(y),
          y_q2.5 = exp(y_q2.5),
          y_q97.5 = exp(y_q97.5))
    } else if (y_scale %in% c("ordinary", "log scale") & log_xplus_1) {
      resultlist$plot_data[[k_sel]] <- resultlist$plot_data[[k_sel]] %>% 
        mutate(
          y = exp(y)-1,
          y_q2.5 = exp(y_q2.5)-1,
          y_q97.5 = exp(y_q97.5)-1)
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


plot_timeseries2a <- function(param, stationcode,
                              tissue = NULL,
                              species = NULL,
                              basis = NULL,
                              y_scale = "ordinary",
                              ymax_perc = 100,
                              xmin_rel = 0, xmax_rel = 0,
                              eqs = TRUE, proref = 1,
                              data = dat_raw5,
                              trend_results = result_list,
                              trend_results_df = result_list_series,
                              data_trend = dat_trend,
                              quantiles = c(0.25, 0.75),
                              medians = TRUE,
                              allsamples = FALSE){
  
  # browser()

  sel2 <- with(
    data_trend,
    PARAM == param & STATION_CODE == stationcode & Basis == basis)
  if (!is.null(tissue))
    sel2 <- sel2 & with(data_trend, TISSUE_NAME %in% tissue)
  if (!is.null(species))
    sel2 <- sel2 & with(data_trend, LATIN_NAME %in% species)
  
  ### -- Get regression result strings      
  trendstring_short <- data_trend[sel2 & data_trend$Trend_type == "short",]$Trend_string
  trendstring_long <- data_trend[sel2 & data_trend$Trend_type == "long",]$Trend_string
  
  if (length(trendstring_long) > 1 | length(trendstring_short) > 1 ){
    txt1 <- paste("Species found:", paste(unique(data_trend[sel2,]$LATIN_NAME), collapse = ", "), "\n")    
    txt2 <- paste("Tissues found:", paste(unique(data_trend[sel2,]$TISSUE_NAME), collapse = ", "), "\n") 
    stop(">1 series selected. You must probably add 'tissue = ...' (see below for tissues at this station) \n\n",
         txt1, txt2)
  }
  
  # c(trendstring_long, trendstring_short)
  
  ### -- Get trend line data    
  
  # 1. get index 'i'  
  sel1 <- with(
    trend_results_df,
    PARAM == param & STATION_CODE == stationcode & Basis == basis)
  if (!is.null(tissue))
    sel1 <- sel1 & data_trend$TISSUE_NAME %in% tissue
  if (!is.null(species))
    sel1 <- sel1 & data_trend$LATIN_NAME %in% species
  i <- which(sel1) 
  
  # 2. get resultlist  
  trend_results_sel <- trend_results[[i]]
  
  # resultlist
  
  ### --- Get raw data  
  raw_data <- data %>%
    filter(PARAM %in% param & STATION_CODE %in% stationcode & Basis %in% basis & !is.na(y))
  if (!is.null(tissue))
    raw_data <- raw_data %>% filter(data_trend$TISSUE_NAME %in% tissue)
  if (!is.null(species))
    raw_data <- raw_data %>% filter(data_trend$LATIN_NAME %in% species)
  
  # raw_data
  # browser()
  
  gg <- plot_timeseries2b(y_scale = y_scale,
                          ymax_perc = ymax_perc,
                          xmin_rel = xmin_rel, xmax_rel = xmax_rel,
                          eqs = eqs, proref = proref,
                          raw_data = raw_data, 
                          resultlist = trend_results_sel,
                          data_trend = data_trend[sel2,],
                          quantiles = quantiles,
                          medians = medians,
                          allsamples = allsamples)
  
  gg
  
}




#
# Extract and plot data from results (on files) and data (in memory)
#
plot_timeseries2b <- function(
                                     y_scale = "ordinary",
                                     ymax_perc = 100,
                                     xmin_rel = 0, xmax_rel = 0,
                                     eqs = TRUE, proref = 1,
                                     raw_data = raw_data, 
                                     resultlist = resultlist,
                                     data_trend = data_trend[sel2,],
                                     quantiles = c(0.25, 0.75),
                                     medians = TRUE,
                                     allsamples = FALSE,
                                     trendtext_size = 4){
  
  # str(resultlist, 1)
  
  # debugonce(get_pointdata)
  df_points <- raw_data %>%
    mutate(y_comb = ifelse(is.na(y), threshold, y))
  
  # EQS and Proref
  include_eqs <- !is.na(df_points$EQS[1]) & eqs
  
  proref_x <- as.numeric(strsplit(proref, split = ",")[[1]])
  include_proref <- !is.na(df_points$Proref[1]) & length(proref_x) > 0
  
  # Get unit to print on y axis  
  unit_print <- get_unit_text(tail(df_points$UNIT, 1), tail(df_points$Basis, 1), tail(df_points$PARAM, 1))
  
  # df_points
  
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
  
  # browser()
  titlestring <- paste0(df_points$Param_name[1], " in ", df_points$Species_name[1], " at ", df_points$Station_name[1])
  subtitlestring <- paste0("Station code: ", resultlist$STATION_CODE, " (region: ", df_points$Region[1], "). ", 
                           str_to_sentence(df_points$Tissue_name[1]), " (basis ", resultlist$Basis, "), ", resultlist$LATIN_NAME)
  
  # Some parameters (VDSI, sums with exloq, etc.) contain zeros and we used log(x+1) instead of log(x) fpr their analysis
  # See 125_extra1_2021.R
  log_xplus_1 <- grepl("VDSI", df_points$PARAM[1]) | grepl("_exloq", df_points$PARAM[1], fixed = TRUE)
  
  if (df_points$transform[1] %in% "log0"){
    backtrans <- function(x) exp(x)
  } else if (df_points$transform[1] %in% "log0"){
    backtrans <- function(x) exp(x)-1
  } else if (df_points$transform[1] %in% "nolog"){
    backtrans <- function(x) x
  }

  if (y_scale %in% c("ordinary", "log scale")){
    df_median <- df_median %>% 
      mutate(
        y = backtrans(y),
        ymin = backtrans(ymin),
        ymax = backtrans(ymax))
    df_points <- df_points %>% 
      mutate(y = backtrans(y),
             threshold = exp(threshold))
  }
  # else if (y_scale %in% c("ordinary", "log scale") & log_xplus_1){
  #   df_median <- df_median %>% 
  #     mutate(
  #       y = exp(y)-1,
  #       ymin = exp(ymin)-1,
  #       ymax = exp(ymax)-1)
  #   df_points <- df_points %>% 
  #     mutate(y = exp(y)-1,
  #            threshold = exp(threshold))
  # }
  
  # list(df_median, df_points)
  
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
    k_sel <- as.character(resultlist$k_sel)   # corrected version
    if (y_scale %in% c("ordinary", "log scale") & !log_xplus_1){
      resultlist$plot_data[[k_sel]] <- resultlist$plot_data[[k_sel]] %>% 
        mutate(
          y = exp(y),
          y_q2.5 = exp(y_q2.5),
          y_q97.5 = exp(y_q97.5))
    } else if (y_scale %in% c("ordinary", "log scale") & log_xplus_1) {
      resultlist$plot_data[[k_sel]] <- resultlist$plot_data[[k_sel]] %>% 
        mutate(
          y = exp(y)-1,
          y_q2.5 = exp(y_q2.5)-1,
          y_q97.5 = exp(y_q97.5)-1)
    }
    gg <- gg +
      geom_ribbon(data = resultlist$plot_data[[k_sel]], aes(ymin = y_q2.5, ymax = y_q97.5), fill = "grey70") +
      geom_line(data = resultlist$plot_data[[k_sel]])
  }
  
  
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

# TEST ----






if (F){
  
  input$param <- "CB153"
  stationcode = "11X"
  input$basis <- "WW"
  

  #tsplot <- 
  debugonce(plot_timeseries2a)
  debugonce(plot_timeseries2b)
  #str(
    plot_timeseries2a(param = input$param, 
                      stationcode = stationcode, 
                      basis = input$basis, 
                      y_scale = input$y_scale,
                      ymax_perc = input$ymax_perc,
                      xmin_rel = input$xmin_rel,
                      xmax_rel = input$xmax_rel,
                      eqs = input$eqs,
                      proref = input$proref,
                      data = dat_all_prep3,
                      trend_results = result_list,
                      trend_results_df = result_list_series,
                      data_trend = dat_series_trend,
                      quantiles = quantiles,
                      medians = input$medians, 
                      allsamples = input$allsamples)
    #, 1)
  
}


bogus <- function(){  
  
  
  

  
    
    
  
}



#
# Get trend string, with percent pro anno increase/decrease
#
get_trendstring <- function(x, trenddata){
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
  # debugonce(get_trendstring)
  get_trendstring("long", test)
  get_trendstring("short", test)
  map_chr(c("long", "short"), get_trendstring, trenddata = test)
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


