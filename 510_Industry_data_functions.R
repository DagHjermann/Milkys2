
#
# read_last_file
#
# Reads the last file with a certain name pattern
# Meant for the MILKLYS data flow  
#

read_last_file <- function(folder = "Data", 
                           pattern = NULL,
                           file = NULL,
                           show_files = 0){
  
  if (!is.null(pattern)){
    files <- dir(folder, pattern = pattern) %>% rev()
    if (show_files > 0){
      cat("The last files files fitting this pattern: \n")
      cat("---------------------------------------------- \n")
      show_files <- min(show_files, length(files))
      cat(files[1:show_files] %>% paste(collapse = "\n"))
      cat("\n")
      cat("---------------------------------------------- \n")
      cat("\n")
    }
  }
  
  if (is.null(file)){
    if (is.null(pattern))
      stop("You must specify either 'pattern' of 'file'")
    cat("Reading the last file downloaded:")
    cat("\n", files[1])
    cat("\n")
    cat("If you want to read a different file, specify 'file' as a number (e.g. 2 if you want to read the\n")
    cat(" second last file) or a text string (for the literal file name)\n")
    cat("Set 'show_files' to x (larger than 1) in order to show the x last files fitting 'pattern'")
    cat("\n\n")
    filename <- files[1]
  } else if (is.numeric(file)){
    if (is.null(pattern))
      stop("You must specify either 'pattern' of 'file'")
    filename <- files[file]
  } else {
    filename <- file
  }
  
  cat("Time since this file was modified: \n ")
  dt <- Sys.time() - file.info(paste0(folder, "/", filename))$mtime
  print(dt)
  
  data <- readRDS(paste0(folder, "/", filename))
  
  # We save the date part of the text (e.g., '2020-04-23')
  # This will be used in part 10, when we save the resulting file
  pos1 <- regexpr("20", filename)[1]   # search for the first place "20" occurs inside filename
  pos2 <- pos1 + 9
  file_date <- substr(filename, pos1, pos2)    # pick out the part of the text from character no. 17 to no. 26
  
  list(data = data, file_date = file_date)
  
}


# Tests / examples
if (FALSE){
  last_file_list <- read_last_file(folder = "Data", pattern = "109_adjusted_data")
  last_file_list <- read_last_file(folder = "Data", pattern = "109_adjusted_data", show_files = 10)
  last_file_list <- read_last_file(folder = "Data", pattern = "109_adjusted_data", file = 5)
  last_file_list <- read_last_file(folder = "Data", file = "109_adjusted_data_2020-05-29")
  head(last_file_list$data)  # Data
  last_file_list$file_date  # Date string (to be used when saving results later)
}


#
# calculate_medians
#
# Calculates medians for all variables
#
# Necessary measurements: VALUE_WW, VALUE_DW, VALUE_FB, VALUE_WWa, VALUE_DWa, VALUE_FBa
# - plus FLAG1, DRYWT, FAT_PERC
# Necessary parameters identifying one sampling occasion: MYEAR, STATION_CODE, LATIN_NAME, TISSUE_NAME
# Necessary parameters identifying measurements: PARAM, UNIT
#

calculate_medians <- function(data){
  # data_all_dt <- lazy_dt(data_all)
  
  t0 <- Sys.time()
  
  data_med_1a <- data %>% 
    group_by(MYEAR, STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM, UNIT) %>% 
    summarise_at(vars(DRYWT, FAT_PERC, VALUE_WW, VALUE_DW, VALUE_FB, VALUE_WWa, VALUE_DWa, VALUE_FBa), 
                 median, 
                 na.rm = TRUE) %>%
    as_tibble()
  
  
  ### Calculate N, Det_limit and Over_LOQ
  
  data_med_1b <- data %>%
    group_by(MYEAR, STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM, UNIT) %>%
    summarise(
      N = n(), 
      Det_limit = median(VALUE_WW[!is.na(FLAG1)]), 
      Over_LOQ = sum(is.na(FLAG1)),
      .groups = "drop") %>%
    as_tibble()
  
  cat("Calculation time: ")
  print(Sys.time() - t0)  # 4 secs
  cat("\n\n")
  
  ### Combine data_med_1 and data_med_2    
  # Add N, Det_limit and Over_LOQ to the data set with medians  
  
  # Make sure the data sets are sorted the same way
  data_med_1a <- data_med_1a %>% arrange(MYEAR, STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM, UNIT)
  data_med_1b <- data_med_1b %>% arrange(MYEAR, STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM, UNIT)
  
  # Make sure the data sets have the same number of rows
  if (nrow(data_med_1a) == nrow(data_med_1b)){
    # Join data by adding the N, Det_limit, Over_LOQ columns 
    data_med_wide <- bind_cols(
      data_med_1a,
      data_med_1b %>% select(N, Det_limit, Over_LOQ)
    )
  } else {
    cat("Data series ('data_med_1a' and 'data_med_1b') have different number of rows! \n")
    cat(" Must be fixed, or use left_join(). \n")
  }
  
  data_med_long <- data_med_wide %>%
    pivot_longer(
      cols = c(VALUE_WW, VALUE_DW, VALUE_FB, VALUE_WWa, VALUE_DWa, VALUE_FBa), 
      names_to = "Basis", 
      values_to = "Median") %>%
    mutate(Basis = stringr::str_sub(Basis, start = 7))
  
  data_med_long

}

if (FALSE){
  data_medians <- calculate_medians(data_all)
}
  



#
# 
#

get_proref <- function(folder = "Input_data"){
  
  # Old ones 
  proref_old <- read_excel(paste0(folder, "/Proref_report_2017.xlsx"))
  proref_paper <- read_excel(paste0(folder, "/Proref_paper.xlsx"))
  
  # proref_updated01 = old data set, with selected columns,
  #   and removing the rows that were produced for the paper (proref_ww)
  proref_updated01 <- proref_old %>%
    # Select columns to keep
    select(PARAM, LATIN_NAME, TISSUE_NAME, Basis, Stations, N_stations, N, Median, Q95) %>%
    # Pick rows that are *not* in proref_paper
    anti_join(proref_paper %>% select(PARAM, LATIN_NAME, TISSUE_NAME, Basis),
              by = c("PARAM", "LATIN_NAME", "TISSUE_NAME", "Basis")
    )
  
  # proref_updated02 - adding the rows from proref_paper
  proref_updated02 <- proref_updated01 %>%
    bind_rows(
      proref_paper %>%
        select(PARAM, LATIN_NAME, TISSUE_NAME, Basis, Stations, N_stations, N, Median, Q95)
    )
  
  # Pick one VDSI line and change it
  sel <- proref_updated02$PARAM == "VDSI" & proref_updated02$Basis == "WW"; sum(sel)
  proref_to_add <- proref_updated02[sel,]
  proref_to_add$PARAM <- "VDSI/Intersex"
  
  # Add 
  proref_updated03 <- bind_rows(
    proref_updated02,
    proref_to_add
  )
  
  proref_updated03

  }




#
# plot_medians_and_trends()
# Function for full-color plots based on one "line" in the data, including trend symbols  
#
# Using
# * get_plotdata
# * get_dfbig 
# * plot_medians_color
# * add_trend   
#
# OVERVIEW:
#   calls get_plotdata() 
#      - gets data from data_median
#        (must contain 'PARAM', 'LATIN_NAME', 'TISSUE_NAME', 'Basis', 'STATION_CODE', 'MYEAR', 'Median')
#      - runs trend analysis using model_from_medians()
#      - returns median data + trend lines as a big list object (X)
#   gets Proref from a separate file 
#     (must contain 'PARAM', 'LATIN_NAME', 'TISSUE_NAME', 'Basis' and 'Limit')
#   gets EQS from a separate file 
#     (must contain 'PARAM' and 'Limit')
#   calls plot_medians_color() with X (for points + trend), proref and EQS (from dfbig)
#      - makes plot, except trend symbols
#      - returns plot (gg) 
#   calls set_symbol_word() to get trends (= 'arrowup', 'circle' etc.)
#   calls add_trend() with gg + trends as input
#      - adds plot symbols to the plot (gg)
#   create file name
#   returns plot (gg) and file name (fn) 




plot_medians_and_trends2 <- function(ser, x_rel = 0.8, y_rel = 0.9, xlim = NULL, ylim = NULL, 
                                    titlesize = 0.85, 
                                    trend_years = NULL, 
                                    data_medians,
                                    data_proref, 
                                    data_eqs,
                                    trendsymbols_x_spacing = c(0, 0.030, 0.065),
                                    windows = FALSE,
                                    log_transform = FALSE,
                                    trendsymbols = TRUE,
                                    ymax_factor = 1.15,
                                    ymin_factor = 0.9,
                                    ...){
  
  if (is.null(trend_years) & !is.null(xlim))
    trend_years <- seq(xlim[1], xlim[2])
  if (is.null(trend_years) & is.null(xlim))
    stop("You must supply either xlim or trend_years")
  X <- get_plotdata(ser[1], ser[2], ser[3], ser[4], ser[5],
                    trend_years = trend_years, 
                    data_medians = data_medians)
  # Used to set x_axis limits:
  start_yr <-  2*floor(min(X$df_data$MYEAR)/2)
  end_yr <-  2*ceiling(max(X$df_data$MYEAR)/2)
  
  time_trend_long <- model_from_medians(ser[1], ser[2], ser[3], ser[4], ser[5],
                                        trend_years, 
                                   data_medians = data_medians, 
                                   ggplot = FALSE)$statistics
  time_trend_10yr <- model_from_medians(ser[1], ser[2], ser[3], ser[4], ser[5],
                                        tail(trend_years, 11),  # 11 because first + last years are included 
                                        data_medians = data_medians, 
                                        ggplot = FALSE)$statistics
  
  # The next is needed because the big excel file for 2018 (made in 2019) uses
  #   English tissue names
  if (FALSE){
    dfbig_tissue <- case_when(
      ser[3] == "Lever" ~ "Liver",
      ser[3] == "Muskel" ~ "Muscle",
      TRUE ~ ser[3])
    dfbig <- get_dfbig(ser[1], ser[2], dfbig_tissue, ser[4], ser[5])
  }
  
  # Proref from median data
  # proref <- X$df_data$Q95[1]
  
  # Proref from a separate file (must contain 'PARAM', 'LATIN_NAME', 'TISSUE_NAME', 'Basis' and 'Limit')
  proref <- data_proref %>%
    filter(PARAM == ser[1] & LATIN_NAME == ser[2] & TISSUE_NAME == ser[3] & Basis == ser[5]) %>% # 4 = station
    pull(Q95)
  if (length(proref) == 0)
    proref <- NA
  
  # EQS from a separate file (must contain 'PARAM' and 'Limit')
  eqs <- data_eqs %>%
    filter(PARAM == ser[1]) %>% 
    pull(Limit)

  # Print to screen
  cat("PROREF = ", proref)
  if (length(eqs) > 0)
    cat(", EQS = ", eqs)
  cat("\n")
  
  # Axis limits - note some hard-coding if xlim = NULL
  if (is.null(xlim))
    xlim <- c(start_yr, end_yr) - 2  # to make room for PROREF text
  if (is.null(ylim)){
    ymax <- max(subset(X$df_data, MYEAR >= xlim[1] & MYEAR <= xlim[2])$Median)*ymax_factor
    if (log_transform){
      ymin <- min(subset(X$df_data, MYEAR >= xlim[1] & MYEAR <= xlim[2])$Median)*ymin_factor
      ylim <- c(ymin, ymax)
    } else {
      ylim <- c(0, ymax)
    }
  }
    
  # This is making the right tissue to use in the plot title
  X$tissue <- case_when(
    X$tissue %in% "Lever" ~ "liver",
    X$tissue %in% "Muskel" ~ "muscle",
    X$tissue %in% "Whole soft body" ~ "soft body",
    TRUE ~ X$tissue
  )
  
  # Title
  plot_title <- paste0(X$paramname, " in ", tolower(X$speciesname), " ", X$tissue, ", ", X$stationname)
  if (ser[5] %in% c("WWa", "DWa", "FBa"))
    plot_title <- paste0(plot_title, ". Note: Length-adjusted concentrations")
  # Insert line breaks if text is longer than 69 characters
  plot_title <- break_text(plot_title, 69, type = "max", 
                           no_break_after = c("st.", "Note:")
  )
  
  # Find breaks to supply to 'scale_x_continuous'
  yrstep <- case_when(
    diff(xlim) < 18 ~ 2,     # show every second year on x axis 
    diff(xlim) >= 18 ~ 4     # show every fourth year on x axis
  )
  
  # Years to show on x axis
  breaks <- seq(
    2*ceiling(xlim[1]/2), 
    2*floor(xlim[2]/2), 
    yrstep)
  
  # Start plot.   ELU: testing "geom_text(aes(x=Inf, y=Inf, hjust=10.0, vjust=2.7, label = "\u25AA"), size = rel(8))"
  gg <- plot_medians_color(X, proref, xlim = xlim, eqs = eqs[1], ylim = ylim, ...) +
    scale_x_continuous(minor_breaks = seq(xlim[1], xlim[2]), breaks = breaks) +
    ggtitle(plot_title) +
    
    #ELU: evt små firkanter som mangler:
    #geom_text(aes(x=Inf, y=Inf, hjust=10.1, vjust=2.6, label = "\u25AA"), size = rel(8)) +
    #geom_text(aes(x=Inf, y=Inf, hjust=8.6, vjust=2.6, label = "\u25AA"), size = rel(8)) +
    
    theme(title = element_text(size = rel(titlesize)))
  
  if (log_transform){
   gg <- gg + scale_y_log10()
  }

  # Add trend symbol
  trends <- set_symbol_word(bind_rows(time_trend_long, time_trend_10yr))
  
  # browser()
  if (trendsymbols){
    gg <- add_trend(gg, trendsymbols = trends, x_rel = x_rel, y_rel = y_rel, 
                    x_spacing = trendsymbols_x_spacing, 
                    fontsize = 10,
                    windows = windows)
  }
  
  # Make file name (but does not save file)
  fn <- paste0("TSplot_", ser[1], "_", ser[2], "_", ser[3], "_", ser[4], "_", ser[5], ".png")
  
  # Return plot (gg) and file name (fn)
  list(gg = gg, fn = fn)
}

# ylim <- 0.44
# X <- plot_medians_and_trends(c("HG", "Gadus morhua", "Muskel", "13B",  "WW"), #
#                              eqs_type = "background", xlim = c(1978, 2018), ylim = c(0, ylim))
# X$gg


#
# Function for setting symbol
# Differs from set_symbol(), which returns the single ASCII sign
#
# up = é      ascii 233  Increasing concentration
# down = ê    ascii 234  Decreasing concentration
# circle = ¢  ascii 162  No significant time trend
# square = §  ascii 167  Too few years to make time trend
# star = «    ascii 171  Too few years with data over LOQ to make time trend

set_symbol_word <- function(data){
  result <- rep("", nrow(data))
  sel <- with(data, !Model_used %in% c("Linear", "Nonlinear") & (is.na(N_data) | N_data < 5))
  result[sel] <- "filledsquare"
  # result[sel] <- chr(167)
  sel <- with(data, !Model_used %in% c("Linear", "Nonlinear") & N_data >= 5)
  result[sel] <- "star"
  # result[sel] <- chr(171)
  sel <- with(data, Model_used %in% c("Linear", "Nonlinear") & P_change > 0.05)
  result[sel] <- "circle"
  if ("Status" %in% names(data)) {  # if this column exists
    sel <- with(data, Status %in% "No variation in data")
    result[sel] <- "circle"
  }
  # result[sel] <- chr(162)
  sel <- with(data, Model_used %in% c("Linear", "Nonlinear") & P_change <= 0.05 & Dir_change %in% "Up")
  result[sel] <- "arrowup"
  # result[sel] <- chr(233)
  sel <- with(data, Model_used %in% c("Linear", "Nonlinear") & P_change <= 0.05 & Dir_change %in% "Down")
  result[sel] <- "arrowdown"
  # result[sel] <- chr(234)
  result
}


round_pvalue <- function(x){
  if (x > 0.2){
    "P > 0.2"
  } else if (x >= 0.05){
    paste("P =", round(x, 2))
  } else if (x >= 0.01){
    "0.01 <= P < 0.05"
  } else if (x >= 0.001){
    "0.001 <= P < 0.01"
  } else if (x < 0.001){
    "P < 0.001"
  }
}


# Function for plotting proref only   
# * Need this in some cases where just changing x and y limits isnn't enough  

plot_medians_proref <- function(X, proref,
                               title = NULL, 
                               include_zero = TRUE, proref_x = NULL, show_proref = 1:5, 
                               xlim = NULL, ylim = NULL, ylim_proref = NULL,
                               trend_color = "black", trend_size = 2,    # For eqs_type = "background", use e.g. dark/light green for trends:
                               ci_fill = "grey80",                        # trend_color = "#33a02c", ci_fill = "#b2df8a"  (rom  PuBuGn)
                               points_color = "black", points_fill = "grey30", points_size = 3, points_shape = 21, 
                               points_color_loq = "black", points_fill_loq = "#a6cee3", points_size_loq = 3, points_shape_loq = 25,  # light blue
                               proref_label_transparent = TRUE,                                        # if TRUE, 'PROREF ...' will be on a white rectangle 
                               proref_label_size = 4,                                                 # Size of 'PROREF ...' label 
                               add_line = FALSE,        # for line through points   
                               add_trend_linear = FALSE,  # for linear regression line
                               add_trend_text = TRUE
){      # red
  
  sel_detlimit <- with(X$df_data, Over_LOQ <= N_median*0.5) 
  
  gg <- ggplot(X$df_data, aes(x = MYEAR))
  
  if (add_line){
    gg <- gg + 
      geom_line(data = X$df_data, aes(y = Median))
  }
  
  if (add_trend_linear){
    gg <- gg + 
      geom_smooth(data = X$df_data, aes(y = Median), method = "lm", se = FALSE)
  }
  
  gg <- gg + 
    geom_point(data = X$df_data[!sel_detlimit,], aes(y = Median), 
               shape = points_shape, color = points_color, fill = points_fill, size = points_size) +
    geom_point(data = X$df_data[sel_detlimit,], aes(y = Median), 
               shape = points_shape_loq, color = points_color_loq, fill = points_fill_loq, size = points_size_loq) +
    scale_x_continuous(breaks = unique(X$df_data$MYEAR), minor_breaks = NULL)

  if (is.null(xlim))
    xlim <- range(X$df_data$MYEAR, na.rm = TRUE) + c(-1,0)
  if (is.null(proref_x)){
    proref_x <- rep(xlim[1], 5)
  }
  
    
  txt_lim <- data.frame(
    MYEAR = proref_x, 
    Median = c(1,2,5,10,20)*proref, 
    txt = c("PROREF", "PROREF x 2", "PROREF x 5", "PROREF x 10", "PROREF x 20")
  )
  
  # PROREF lines
  if (!is.na(proref) & proref_label_transparent){
    for (i in show_proref){
      gg <- gg +
        geom_hline(yintercept = txt_lim[i, "Median"], linetype = 2, col = "grey25") +
        geom_text(data = txt_lim[show_proref,], aes(y = Median, label = txt), hjust = 0, vjust = -0.5, size = proref_label_size)
    }
      
    } else if (!is.na(proref) & !proref_label_transparent){
      for (i in show_proref){
        gg <- gg +
          geom_hline(yintercept = txt_lim[i, "Median"], linetype = 2, col = "grey25") +
          geom_label(data = txt_lim[show_proref,], aes(y = Median, label = txt), hjust = 0, vjust = -0.5, size = proref_label_size, label.size = 0)
      }
    }

  
  if (add_trend_text){
    mod <- lm(Median ~ MYEAR, X$df_data)
    trend_coef <- summary(mod)$coef["MYEAR",]
    trend_text <- paste0(
      "Trend: ", round(trend_coef["Estimate"], 3), "/yr", 
      " (", round_pvalue(trend_coef["Pr(>|t|)"]), ")"
    )
    gg <- gg + annotate("label", x = Inf, y = Inf, label = trend_text, 
                        hjust = 1.5, vjust = 1.5,
                        size = 4)
  }
  
  gg

  
}



#
# Used in 512_Hvaler_Fucus_bluemussel
#

get_trendsymbol <- function(trendsymbol, windows = FALSE){
  library(extrafont)
  # For Windows PCs
  if (windows){
    txt <- case_when(
      trendsymbol == "arrowup" ~ "\u2191",    # Wingdings 3, h
      trendsymbol == "arrowdown" ~ "\u2193",  # Wingdings 3, i
      trendsymbol == "circle" ~ "\u26AA",     # Wingdings, U+2B58 
      trendsymbol == "star" ~ "\u2606",
      trendsymbol == "square" ~ "\u25FE"
    )
    fontsize_rel <- case_when(
      trendsymbol == "arrowup" ~ 0.8,
      trendsymbol == "arrowdown" ~ 0.8,
      trendsymbol == "circle" ~ 0.7,
      trendsymbol == "star" ~ 1,
      trendsymbol == "square" ~ 1.3
    )
    # For Linux (e.g. Jupyterhub)
  } else {
    txt <- case_when(
      trendsymbol == "arrowup" ~ "\u2191",
      trendsymbol == "arrowdown" ~ "\u2193",
      trendsymbol == "circle" ~ "\u25CB",
      trendsymbol == "star" ~ "\u2606",
      trendsymbol == "filledsquare" ~ "\u25FE"  # 25FE
    )
    relsize <- case_when(
      trendsymbol == "filledsquare" ~ 0.55,
      trendsymbol == "arrowdown" ~ 0.8,
      trendsymbol == "arrowup" ~ 0.8,
      trendsymbol == "star" ~ 0.65,
      TRUE ~ 1
    )
  }
  
  list(txt, relsize)
  
}


select_data <- function(ser, data_medians){
  data_medians %>%
    filter(PARAM == ser[1] & LATIN_NAME == ser[2] & TISSUE_NAME == ser[3] & 
             STATION_CODE == ser[4] & Basis == ser[5])
}

plot_medians_and_trends3 <- function(ser, x_rel = 0.8, y_rel = 0.9, xlim = NULL, ylim = NULL, 
                                     titlesize = 0.85, 
                                     trend_years = NULL, 
                                     data_medians,
                                     fontsize = 5,
                                     trend_hjust = 1.3, trend_vjust = 1.3, trend_size = 12){
  
  if (is.null(trend_years) & !is.null(xlim))
    trend_years <- seq(xlim[1], xlim[2])
  if (is.null(trend_years) & is.null(xlim))
    stop("You must supply either xlim or trend_years")
  
  df_data <- select_data(ser=ser, data_medians=data_medians)
  
  obj <- list()
  obj$df_med_st <- df_data %>% mutate(MYEAR_regr = MYEAR)
  obj$sel_ts <- rep(TRUE, 10)
  obj$N <- nrow(obj$df_med_st)
  obj$Nplus <- nrow(obj$df_med_st)
  regr_result <- calc_models_gam(obj, gam = TRUE, n_pred = 30)
  time_trend <- statistics_for_excel(obj, regr_result, gam = TRUE) 
  time_trend$N_data <- nrow(obj$df_med_st)
  
  # Add trend symbol
  trend <- set_symbol_word(time_trend)
  trendsymbol <- get_trendsymbol(trend)
  
  # Title
  plot_title <- paste0(ser[1], " i ", ser[2], ", stasjon ", ser[4])
  
  # y limit
  ylim <- c(0, max(df_data$Median, na.rm = TRUE)*1.3)
  
  # Start plot
  gg <- ggplot(df_data, aes(x = MYEAR))
  
  # Add trend
  if (time_trend$AICc_nonlin[1] < time_trend$AICc_lin[1]){
    # Non-linear trend
    gg <- gg +
      geom_ribbon(data = regr_result$mod_lin_yFit, aes(x = Year, ymin = exp(LowLimit), ymax = exp(HighLimit)), fill = "lightsteelblue") + 
      geom_line(data = regr_result$mod_lin_yFit, aes(x = Year, y = exp(Estimate)))
  } else {
    # Linear trend
    gg <- gg +
      geom_smooth(aes(y = Median), method = "lm", fill = "lightsteelblue")
  }

  # Add points and the rest
  gg <- gg +
    geom_point(aes(y = Median)) +
    labs(title = plot_title) +
    coord_cartesian(ylim = ylim ) +
    annotate("text", x = Inf, y = Inf, label = trendsymbol[[1]],  
             hjust = trend_hjust, vjust = trend_vjust, size = trend_size*trendsymbol[[2]])
  
  ## Add theme
  gg <- gg + theme_bw()
  
  # Make file name (but does not save file)
  fn <- paste0("TSplot_", ser[1], "_", ser[2], "_", ser[3], "_", ser[4], "_", ser[5], ".png")
  fn <- tolower(fn)
  fn <- sub("æ", "ae", fn)
  fn <- sub("ø", "oe", fn)
  fn <- sub("å", "aa", fn)
  
  # Return plot (gg) and file name (fn)
  list(gg = gg, fn = fn)
  
}




