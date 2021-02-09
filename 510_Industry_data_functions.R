

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
#      - gets data from data_med2
#      - runs trend analysis using model_from_medians()
#      - returns median data + trend lines as a big list object (X)
#   calls get_dfbig()
#      - gets the relevant line from the big excel file
#      - gets EQS and trend symbols from here
#      - returns dfbig = one line of the big excel file
#   calls plot_medians_color() with X (for points + trend), proref and EQS (from dfbig)
#      - makes plot, except trend symbols
#      - returns plot (gg) 
#   creates trends (= arrowup, circle etc.)
#   calls add_trend() with gg + trends as input
#      - adds plot symbols to the plot (gg)
#   create file name
#   returns plot (gg) and file name (fn) 


plot_medians_and_trends <- function(ser, x_rel = 0.8, y_rel = 0.9, xlim = NULL, ylim = NULL, 
                                    titlesize = 0.85, trend_years = NULL, ...){
  
  if (is.null(trend_years) & !is.null(xlim))
    trend_years <- seq(xlim[1], xlim[2])
  if (is.null(trend_years) & is.null(xlim))
    stop("You must supply either xlim or trend_years")
  X <- get_plotdata(ser[1], ser[2], ser[3], ser[4], ser[5],
                    trend_years = trend_years)
  start_yr <-  2*floor(min(X$df_data$MYEAR)/2)
  
  # The next is needed because the big excel file for 2018 (made in 2019) uses
  #   English tissue names
  dfbig_tissue <- case_when(
    ser[3] == "Lever" ~ "Liver",
    ser[3] == "Muskel" ~ "Muscle",
    TRUE ~ ser[3])
  dfbig <- get_dfbig(ser[1], ser[2], dfbig_tissue, ser[4], ser[5])
  
  # Last year
  last_year <- grep("Yr", names(dfbig), value = TRUE) %>% sub("Yr_", "", .)  %>% as.numeric() %>% max()
  
  # Proref from median data
  # proref <- X$df_data$Q95[1]
  
  # Proref from big excel data
  proref <- dfbig$Q95[1]
  
  cat("PROREF = ", proref, ", EQS = ", dfbig$EQS, "\n")
  
  # Axis limits - note some hard-coding if xlim = NULL
  if (is.null(xlim))
    xlim <- c(start_yr,2020) - 2  # to make room for PROREF text
  if (is.null(ylim))
    ylim <- c(0, max(X$df_data$Median)*1.15)
  
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
  
  gg <- plot_medians_color(X, proref, xlim = xlim, eqs = dfbig$EQS, ylim = ylim, ...) +
    scale_x_continuous(minor_breaks = seq(xlim[1], xlim[2]), breaks = breaks) +
    ggtitle(plot_title) +
    
    #ELU: evt små firkanter som mangler:
    #geom_text(aes(x=Inf, y=Inf, hjust=10.1, vjust=2.6, label = "\u25AA"), size = rel(8)) +
    #geom_text(aes(x=Inf, y=Inf, hjust=8.6, vjust=2.6, label = "\u25AA"), size = rel(8)) +
    
    theme(title = element_text(size = rel(titlesize)))
  
  # Add trend symbols. 
  trend_variable <- paste0("Trends.", last_year)
  trendsymbol_xl <- c(substr(dfbig[[trend_variable]], 1, 1), substr(dfbig[[trend_variable]], 3, 3))
  trends <- case_when(
    trendsymbol_xl %in% "¢" ~ "circle",
    trendsymbol_xl %in% "é" ~ "arrowup",
    trendsymbol_xl %in% "ê" ~ "arrowdown",
    trendsymbol_xl %in% "§" ~ "filledsquare",
  )
  
  gg <- add_trend(gg, trendsymbols = trends, x_rel = x_rel, y_rel = y_rel, 
                  x_spacing = c(0, 0.035, 0.065), fontsize = 10)
  
  
  # Make file name (but does not save file)
  fn <- paste0("TSplot_", ser[1], "_", ser[2], "_", ser[3], "_", ser[4], "_", ser[5], ".png")
  
  # Return plot (gg) and file name (fn)
  list(gg = gg, fn = fn)
}

# ylim <- 0.44
# X <- plot_medians_and_trends(c("HG", "Gadus morhua", "Muskel", "13B",  "WW"), #
#                              eqs_type = "background", xlim = c(1978, 2018), ylim = c(0, ylim))
# X$gg



