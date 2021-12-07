
cb7 <- c("CB28", "CB52", "CB101", "CB118", "CB138", "CB153", "CB180")

stations_bluemussel <- c(
  "30A", "I301", "I304", "31A", "36A", "I023", "I024",     # Blue mussel, Oslofjorden
  "71A", "I714", "76A2", "I131A", "I133", "15A",           # Blue mussel, Grenland - Sørlandet
  "51A", "52A", "56A", "57A", "64A", "65A", "22A",         # Blue mussel, Vestlandet - Trøndelag
  "I241", "26A2", "28A2", "91A2",                          #  - " -
  "97A2", "97A3", "98A2", "10A2", "11X")                   # Blue mussel, Nord-Norge
  
  
ser_view_med <- function(param = "PYR1OH", st = "15B"){
  data_med %>%
    filter(Basis == "WW" & PARAM == param & STATION_CODE == st) %>%
    arrange(STATION_CODE, MYEAR) %>%
    View(paste(paste(param, collapse = ","), st))
}
# view()
# ser_view_med("CB_S7", "30A")

ser_plot_med <- function(param = "PYR1OH", st = "15B", log = FALSE){
  gg <- data_med %>%
    filter(Basis == "WW" & PARAM %in% param & STATION_CODE == st) %>%
    mutate(Prop_over_LOQ = Over_LOQ/N_median) %>%
    ggplot(aes(MYEAR, Median)) +
    geom_point(aes(color = Prop_over_LOQ)) +
    facet_wrap(vars(PARAM))
  if (log)
    gg <- gg + scale_y_log10()
  gg + labs(title = st)
}
# ser_plot_med("CB_S7", "30A")
# ser_plot_med(cb7, "30A")

ser_plot_raw <- function(param = "PYR1OH", st = "15B", log = FALSE){
  gg <- data_all %>%
    filter(PARAM %in% param & STATION_CODE == st) %>%
    mutate(Over_LOQ = is.na(FLAG1)) %>%
    ggplot(aes(MYEAR, VALUE_WW)) +
    geom_point(aes(color = Over_LOQ)) +
    facet_wrap(vars(PARAM))
  if (log)
    gg <- gg + scale_y_log10()
  gg + labs(title = st)
}
# ser_plot_med("CB_S7", "30A")
# ser_plot_raw(cb7, "30A")

ser_plot_cb <- function(st = "30A"){
  cb7 <- c("CB28", "CB52", "CB101", "CB118", "CB138", "CB153", "CB180")
  gg <- data_med %>%
    filter(Basis == "WW" & PARAM %in% cb7 & STATION_CODE == st) %>%
    mutate(
      PARAM = factor(PARAM, levels = cb7),
      Prop_over_LOQ = Over_LOQ/N_median
      ) %>%
    ggplot(aes(MYEAR, Median)) +
    geom_point(aes(color = Prop_over_LOQ)) +
    facet_wrap(vars(PARAM))
  gg + labs(title = st)
}


ser_plot_cb_raw <- function(st = "30A", log = TRUE){
  cb7 <- c("CB28", "CB52", "CB101", "CB118", "CB138", "CB153", "CB180")
  gg <- data_all %>%
    filter(PARAM %in% cb7 & STATION_CODE == st) %>%
    filter(MYEAR >= 2005) %>%
    mutate(
      PARAM = factor(PARAM, levels = cb7),
      Over_LOQ = is.na(FLAG1)) %>%
    ggplot(aes(MYEAR, VALUE_WW)) +
    geom_point(aes(color = Over_LOQ), size = 1) +
    facet_wrap(vars(PARAM))
  if (log)
    gg <- gg + scale_y_log10()
  gg + labs(title = st)
}
# ser_plot_cb_raw("30A")

ser_trend <- function(param = "PYR1OH", st = "15B"){
  
  bind_rows(
    result_long %>%
      filter(Basis == "WW" & PARAM %in% param & STATION_CODE %in% st),
    result_10yr %>%
      filter(Basis == "WW" & PARAM %in% param & STATION_CODE %in% st)
  )
  }

ser_trend_view <- function(param = "PYR1OH", st = "15B"){
  ser_trend(param = param, st = st) %>%
    View(paste(paste(param, collapse = ","), st))
}


ser_trend_cb10yr <- function(st = "15B"){
  
  cb7 <- c("CB28", "CB52", "CB101", "CB118", "CB138", "CB153", "CB180")
  result_10yr %>%
    filter(Basis == "WW" & PARAM %in% cb7 & STATION_CODE %in% st) %>%
    mutate(PARAM = factor(PARAM, levels = cb7)) %>%
    arrange(as.numeric(PARAM))
  
}
# ser_trend_cb10yr("30A")





list_and_read_rds_file <- function(folder, pattern,  
                                   filenumber = 1, check_date = NULL){
  
  date_pattern <- "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]"
  pattern_mod <- sub("[:date:]", date_pattern, pattern, fixed = TRUE)
  
  files <- dir(path = folder, 
               pattern = pattern_mod) %>% sort(decreasing = TRUE)
  cat("\n")
  data_list <- read_rds_file(folder = folder, 
                             files, 
                             filenumber = filenumber, 
                             get_date = TRUE,
                             time_since_modified = TRUE)
  result <- data_list$data
  if (!is.null(check_date)){
    if (data_list$file_date != check_date){
      stop("Date of this data =", data_list$file_date, "\n",
           "Date of file in check_date =", check_date, "\n",
           "They are different!")
    }
  }
  
  result
}



