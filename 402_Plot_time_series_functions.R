


#
# Extract and plot data from results (on files) and data (in memory)
#
# This function is built on 'tsplot_param' from script 125..functions, but has been adapted
#
plot_timeseries <- function(param, stationcode,
                            tissue = NULL,
                            species = NULL,
                            folder,
                            data = dat_all_prep3, 
                            data_series = dat_series_trend,
                            data_trend = NULL,
                            allsamples = FALSE){
  
  # browser()
  
  seriesno <- get_seriesno(
    param = param, 
    stationcode = stationcode,
    tissue = tissue,
    species = species,
    data_series = data_series)
  
  if (length(seriesno) > 1){
    rawdata <- extract_raw_data(seriesno, data = data, data_series = data_series)
    txt1 <- paste("Species found:", paste(unique(rawdata$LATIN_NAME), collapse = ", "), "\n")    
    txt2 <- paste("Tissues found:", paste(unique(rawdata$TISSUE_NAME), collapse = ", "), "\n") 
    stop(">1 series selected. You must probably add 'tissue = ...' (see below for tissues at this station) \n\n",
         txt1, txt2)
  }
  
  gg <- tsplot_seriesno(seriesno,
                        folder = folder,
                        data = data, 
                        data_series = data_series,
                        data_trend = data_trend,
                        allsamples = allsamples)
  
  gg
  
}

if (FALSE){
  
  debugonce(tsplot_param)
  debugonce(tsplot_seriesno)
  tsplot_param("CB153", "11X", folder = folder_results)
  tsplot_param("CB153", "19N", folder = folder_results)
  
}


#
# Extract and plot data from results (on files) and data (in memory)
#
tsplot_seriesno <- function(seriesno,
                            folder,
                            data = dat_all_prep3, 
                            data_series = dat_series_trend,
                            data_trend = NULL,
                            allsamples = FALSE,
                            trendtext_size = 4){
  
  # browser()
  
  fn <- sprintf("trend_%04.0f.rda", seriesno)
  resultlist <- readRDS(paste0(folder, "/", fn))
  
  # str(resultlist, 1)
  
  # debugonce(get_pointdata)
  df_points <- extract_raw_data(seriesno, data = data, data_series = data_series) %>%
    mutate(y_comb = ifelse(is.na(y), threshold, y))
  
  df_median <- df_points %>%
    group_by(x) %>%
    summarise(
      y = median(y_comb, na.rm = TRUE), 
      n = n(), 
      n_overLOQ = sum(is.na(threshold)),
      ymin = quantile(y_comb, probs = 0.25),
      ymax = quantile(y_comb, probs = 0.75),
      .groups = "drop") %>%
    mutate(overLOQ = n_overLOQ > (0.5*n))
  
  # titlestring <- paste0(resultlist$PARAM, " (", resultlist$Basis, ") at ", resultlist$STATION_CODE, " (", resultlist$TISSUE_NAME, " from ", resultlist$LATIN_NAME, ")")
  titlestring <- paste0(resultlist$PARAM, " at ", resultlist$STATION_CODE, " (", resultlist$TISSUE_NAME, " from ", resultlist$LATIN_NAME, ")")
  
  gg <- ggplot(df_median, aes(x, y))
  
  if (!is.null(resultlist$plot_data)){
    k_sel <- resultlist$k_sel
    gg <- gg +
      geom_ribbon(data = resultlist$plot_data[[k_sel]], aes(ymin = y_q2.5, ymax = y_q97.5), fill = "grey70") +
      geom_line(data = resultlist$plot_data[[k_sel]])
  }
  
  if (allsamples){
    gg <- gg +
      geom_point(data = df_points %>% filter(!is.na(y))) +
      geom_point(data = df_points %>% filter(!is.na(threshold)), aes(y = threshold), shape = 6)
  }
  
  gg <- gg +
    geom_point(data = df_median %>% filter(overLOQ), shape = 21, fill = "red2", size = rel(3)) +
    geom_point(data = df_median %>% filter(!overLOQ), shape = 25, fill = "red2", size = rel(3)) +
    geom_linerange(data = df_median, aes(ymin = ymin, ymax = ymax), color = "red2") +
    labs(title = titlestring) +
    theme_bw()
  
  if (!is.null(data_trend)){
    df_trend_sel <- data_trend %>% 
      filter(PARAM %in% resultlist$PARAM,
             STATION_CODE %in% resultlist$STATION_CODE,
             TISSUE_NAME %in% resultlist$TISSUE_NAME,
             LATIN_NAME %in% resultlist$LATIN_NAME) %>%
      as.data.frame()
    trendstring <- paste0(
      "Long-term: ", subset(df_trend_sel, Trend_type == "long")$Trend_string, "\n",
      "Short-term: ", subset(df_trend_sel, Trend_type == "short")$Trend_string)
    gg <- gg +
      annotate("text", x = Inf, y = Inf, label = trendstring, hjust = 1.1, vjust = 1.2, size = trendtext_size, colour = "blue3")
  }
  
  gg
  
}


