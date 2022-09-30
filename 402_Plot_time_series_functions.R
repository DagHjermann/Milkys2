


#
# Extract and plot data from results (on files) and data (in memory)
#
# This function is built on 'tsplot_param' from script 125..functions, but has been adapted
#
plot_timeseries <- function(param, stationcode,
                            tissue = NULL,
                            species = NULL,
                            y_scale = "ordinary",
                            ymax_perc = 100,
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
  
  gg <- plot_timeseries_seriesno(seriesno,
                                 y_scale = y_scale,
                                 ymax_perc = ymax_perc,
                                 folder = folder,
                                 data = data, 
                                 data_series = data_series,
                                 data_trend = data_trend,
                                 allsamples = allsamples)
  
  gg
  
}

if (FALSE){
  
  debugonce(plot_timeseries)
  debugonce(plot_timeseries_seriesno)
  plot_timeseries("CB153", "11X", folder = folder_results)
  plot_timeseries("CB153", "19N", folder = folder_results)
  
}


#
# Extract and plot data from results (on files) and data (in memory)
#
plot_timeseries_seriesno <- function(seriesno,
                                     y_scale = "ordinary",
                                     ymax_perc = 100,
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
  
  # Get unit to print on y axis  
  unit_print <- get_unit_text(tail(df_points$UNIT, 1), "WW", tail(df_points$PARAM, 1))
  
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
  
  if (y_scale %in% c("ordinary", "log scale")){
    df_median <- df_median %>% 
      mutate(
        y = exp(y),
        ymin = exp(ymin),
        ymax = exp(ymax))
  }
  rn <- c(min(df_median$ymin), max(df_median$ymax))
  y_limits <- c(rn[1], rn[1] + (rn[2]-rn[1])*ymax_perc/100)
    
  gg <- ggplot(df_median, aes(x, y))
  
  if (!is.null(resultlist$plot_data)){
    k_sel <- resultlist$k_sel
    if (y_scale %in% c("ordinary", "log scale")){
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
    if (y_scale == "log scale"){
      gg <- gg +
        scale_y_log10()
    }
    gg <- gg +
      coord_cartesian(ylim = y_limits) +
      labs(y = unit_print, x = "") +
      annotate("text", x = Inf, y = Inf, label = trendstring, hjust = 1.1, vjust = 1.2, size = trendtext_size, colour = "blue3")
  }
  
  gg
  
}

#
# Given unit, basis and param, make a suitable "unit text" for y axis label
#
get_unit_text <- function(unit, basis, param){
  # Unit
  if (param %in% "VDSI"){                                  # special treatment of VDSI
    unit_print <- "VDSI"                                    #endret fra "VDS average"
  } else if (basis %in% "WW" & unit %in% "MG_P_KG"){       # if it is something else than VDSI, we check basis and unit
    unit_print <- "mg/kg (w.w.)"
  } else if (basis %in% "WW" & unit %in% "UG_P_KG"){
    unit_print <- expression(mu*g/kg~(w.w.))
  } else if (basis %in% "DW" & unit %in% "MG_P_KG"){
    unit_print <- "mg/kg (d.w.)"
  } else if (basis %in% "DW" & unit %in% "UG_P_KG"){
    unit_print <- expression(mu*g/kg~(d.w.))
  } else if (basis %in% "FB" & unit %in% "MG_P_KG"){
    unit_print <- "mg/kg (lipid basis)"
  } else if (basis %in% "FB" & unit %in% "UG_P_KG"){
    unit_print <- expression(mu*g/kg~(lipid~basis))
  } else if (basis %in% "WWa" & unit %in% "MG_P_KG"){
    unit_print <- "mg/kg (w.w.) for 50 cm cod"
  } else if (basis %in% "WWa" & unit %in% "UG_P_KG"){
    unit_print <- expression(mu*g/kg~(w.w.)~plain("for 50 cm cod"))
  } else if (basis %in% "DWa" & unit %in% "MG_P_KG"){
    unit_print <- "mg/kg (d.w.) for 50 cm cod"
  } else if (basis %in% "DWa" & unit %in% "UG_P_KG"){
    unit_print <- expression(mu*g/kg~(d.w.)~plain("for 50 cm cod"))
  } else if (basis %in% "FBa" & unit %in% "MG_P_KG"){
    unit_print <- "mg/kg (lipid basis) for 50 cm cod"
  } else if (basis %in% "FBa" & unit %in% "UG_P_KG"){
    unit_print <- expression(mu*g/kg~(lipid~basis)~plain("for 50 cm cod"))
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


