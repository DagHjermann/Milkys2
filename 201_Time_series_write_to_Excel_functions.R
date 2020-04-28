
####################################################################################################
#
# Factor conversion
#
####################################################################################################

#
# Factor to numeric
#
fact2num <- function(x) as.numeric(levels(x)[as.numeric(x)])

#
# Factor to character
#
fact2char <- function(x) levels(x)[as.numeric(x)]

#
# Change all factor variables to characters
#
fact2char_df <- function(df){
  for (i in 1:length(df)){
    if (class(df[[i]])[1] %in% "factor")
      df[[i]] <- fact2char(df[[i]])
  }
  df
}

####################################################################################################
#
# Function for making trend symbol column
#
#####################################################################################################

# Function for setting symbol
#
# up = é      ascii 233  Increasing concentration
# down = ê    ascii 234  Decreasing concentration
# circle = ¢  ascii 162  No significant time trend
# square = §  ascii 167  Too few years to make time trend
# star = «    ascii 171  Too few years with data over LOQ to make time trend

set_symbol <- function(data){
  library(gtools)
  result <- rep("", nrow(data))
  sel <- with(data, !Model_used %in% c("Linear", "Nonlinear") & (is.na(N_data) | N_data < 5))
  # result[sel] <- "§"
  result[sel] <- chr(167)
  sel <- with(data, !Model_used %in% c("Linear", "Nonlinear") & N_data >= 5)
  # result[sel] <- "«"
  result[sel] <- chr(171)
  sel <- with(data, Model_used %in% c("Linear", "Nonlinear") & P_change > 0.05)
  # result[sel] <- "¢"
  result[sel] <- chr(162)
  sel <- with(data, Model_used %in% c("Linear", "Nonlinear") & P_change <= 0.05 & Annual_change > 0)
  # result[sel] <- "é"
  result[sel] <- chr(233)
  sel <- with(data, Model_used %in% c("Linear", "Nonlinear") & P_change <= 0.05 & Annual_change < 0)
  # result[sel] <- "ê"
  result[sel] <- chr(234)
  result
  }

####################################################################################################
#
# make_trend_data_for_excel - Put the trend data in the same order as the Excel file, make some columns
#
####################################################################################################

make_trend_data_for_excel <- function(trend_data, excel_ids){
  # Put the trend data in the same order as the Excel file with background/level data
  trenddata_for_excel <-  left_join(excel_ids, trend_data,  by = c("PARAM", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE"))
  # Define some columns
  trenddata_for_excel$Annual_change_lin <- with(trenddata_for_excel, 100*(exp((Lin_yr2-Lin_yr1)/(Year2-Year1))-1))
  trenddata_for_excel$Annual_change_nlin <- with(trenddata_for_excel, 100*(exp((Nonlin_yr2-Nonlin_yr1)/(Year2-Year1))-1))
  trenddata_for_excel$Annual_change <- with(trenddata_for_excel, ifelse(Model_used %in% "Linear", Annual_change_lin, Annual_change_nlin))
  # Set trend to blank if P > 0.05 - we skip this now
  # trenddata_for_excel$Annual_change <- with(trenddata_for_excel, ifelse(P_change <= 0.05, Annual_change, NA))
  # Add symbol
  trenddata_for_excel$Symbol <- set_symbol(trenddata_for_excel)
  trenddata_for_excel %>% as.data.frame()
  }

# trend_dw_raw_long_for_excel <- make_trend_data_for_excel(trend_long_dw_raw, excel_ids_dw)
# trend_dw_raw_10yr_for_excel <- make_trend_data_for_excel(trend_10yr_dw_raw, excel_ids_dw)

####################################################################################################
#
# make_trend_data_for_excel2 - as 'make_trend_data_for_excel' but also joins by 'Basis' (= Variable)
#
####################################################################################################

make_trend_data_for_excel2 <- function(trend_data, excel_ids){
  # Put the trend data in the same order as the Excel file with background/level data
  trenddata_for_excel <-  left_join(excel_ids, trend_data,  by = c("PARAM", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE", "Basis"))
  # Define some columns
  trenddata_for_excel$Annual_change_lin <- with(trenddata_for_excel, 100*(exp((Lin_yr2-Lin_yr1)/(Year2-Year1))-1))
  trenddata_for_excel$Annual_change_nlin <- with(trenddata_for_excel, 100*(exp((Nonlin_yr2-Nonlin_yr1)/(Year2-Year1))-1))
  trenddata_for_excel$Annual_change <- with(trenddata_for_excel, ifelse(Model_used %in% "Linear", Annual_change_lin, Annual_change_nlin))
  # Set trend to blank if P > 0.05 - we skip this now
  # trenddata_for_excel$Annual_change <- with(trenddata_for_excel, ifelse(P_change <= 0.05, Annual_change, NA))
  # Add symbol
  trenddata_for_excel$Symbol <- set_symbol(trenddata_for_excel)
  trenddata_for_excel %>% as.data.frame()
  }

# trend_dw_raw_long_for_excel <- make_trend_data_for_excel(trend_long_dw_raw, excel_ids_dw)
# trend_dw_raw_10yr_for_excel <- make_trend_data_for_excel(trend_10yr_dw_raw, excel_ids_dw)

####################################################################################################
#
# Combines the output of long- and short termdata made by 'make_trend_data_for_excel()' 
#   into a single data frame ready to be written to Excel and copy-pasted into the 
#   existing Excel file with background/level data
#
####################################################################################################

combine_long_and_short_trends_for_excel <- function(trend_long, trend_short){
    # Define how to change names to the names we want in Excel 
  # One table for long term trends, another for short-term trends 
  cn_change_long <- data.frame(
    old = c("P_change", "Annual_change", "Year1", "Year2", "N", "Symbol"), 
    new = c("Trend p(long)", "Detectable % change(long)", "First Year(long)", "Last Year(long)", "No of Years(long)", "Symbol_long"), stringsAsFactors = FALSE)
  cn_change_short <- data.frame(
    old = c("P_change", "Annual_change", "Year1", "Year2", "N", "Symbol"), 
    new = c("Trend p(short)", "Detectable % change(short)", "First Year(short)", "Last Year(short)", 
          "No of Years(short)", "Symbol_short"), stringsAsFactors = FALSE)
  
  #
  # Create 'data_trends' by picking variables for trend_long, change names
  #
  cn <- c("PARAM", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE", "P_change", "Annual_change", "Year1", "Year2", "N", "Symbol")
  data_trends <- trend_long[,cn] %>% as.data.frame()
  
  # change names of long-term columns
  for (i in 1:nrow(cn_change_long)){
    sel <- colnames(data_trends) %in% cn_change_long[i,"old"]
    colnames(data_trends)[sel] <- cn_change_long[i,"new"]
    }
  
  #
  # add to data_trends: Pick variables for trend_10yr, add to data_trends, change names
  #
  data_trends <- cbind(data_trends, trend_short[,c("P_change", "Annual_change", "Year1", "Year2", "N", "Symbol")]) %>% as.data.frame()
  # nrow(data_trends )
  
  # Change column names of short-term columns
  for (i in 1:nrow(cn_change_short)){
    sel <- colnames(data_trends) %in% cn_change_short[i,"old"]
    colnames(data_trends)[sel] <- cn_change_short[i,"new"]
    }
  
  #
  # Make combined long/short-term symbol
  #
  data_trends$Trend.2016 <- with(data_trends, paste0(Symbol_long, "/", Symbol_short))
  data_trends$Symbol_long <- NULL
  data_trends$Symbol_short <- NULL
  data_trends
  }

####################################################################################################
#
# As above but includes Basis
#
####################################################################################################

combine_long_and_short_trends_for_excel2 <- function(trend_long, trend_short){
    # Define how to change names to the names we want in Excel 
  # One table for long term trends, another for short-term trends 
  cn_change_long <- data.frame(
    old = c("P_change", "Annual_change", "Year1", "Year2", "N", "Symbol"), 
    new = c("Trend p(long)", "Detectable % change(long)", "First Year(long)", "Last Year(long)", "No of Years(long)", "Symbol_long"), stringsAsFactors = FALSE)
  cn_change_short <- data.frame(
    old = c("P_change", "Annual_change", "Year1", "Year2", "N", "Symbol"), 
    new = c("Trend p(short)", "Detectable % change(short)", "First Year(short)", "Last Year(short)", 
          "No of Years(short)", "Symbol_short"), stringsAsFactors = FALSE)
  
  #
  # Create 'data_trends' by picking variables for trend_long, change names
  #
  cn <- c("PARAM", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE", "Basis", "P_change", "Annual_change", "Year1", "Year2", "N", "Symbol")
  data_trends <- trend_long[,cn] %>% as.data.frame()
  
  # change names of long-term columns
  for (i in 1:nrow(cn_change_long)){
    sel <- colnames(data_trends) %in% cn_change_long[i,"old"]
    colnames(data_trends)[sel] <- cn_change_long[i,"new"]
    }
  
  #
  # add to data_trends: Pick variables for trend_10yr, add to data_trends, change names
  #
  data_trends <- cbind(data_trends, trend_short[,c("P_change", "Annual_change", "Year1", "Year2", "N", "Symbol")]) %>% as.data.frame()
  # nrow(data_trends )
  
  # Change column names of short-term columns
  for (i in 1:nrow(cn_change_short)){
    sel <- colnames(data_trends) %in% cn_change_short[i,"old"]
    colnames(data_trends)[sel] <- cn_change_short[i,"new"]
    }
  
  #
  # Make combined long/short-term symbol
  #
  data_trends$Trend.year <- with(data_trends, paste0(Symbol_long, "/", Symbol_short))    # was 'Trend.2016' (changed 2019)
  data_trends$Symbol_long <- NULL
  data_trends$Symbol_short <- NULL
  data_trends
  }


####################################################################################################
#
# Prepare df of trends from previous year (I)
#
####################################################################################################

get_trends_previous_year <- function(trenddata, basis){
  cns <- c("Parameter.Code", "Species", "Tissue", "Station.Code", "Trend.p.long.", "Detectable...change.long.", "First.Year.long.", "Last.Year.long.", 
    "No.of.Years.long.", "Trend.p.short.", "Detectable...change.short.", 
    "First.Year.short.", "Last.Year.short.", "No.of.Years.short.", "Trends.2015")
  trenddata <- results_2015[results_2015$Basis %in% basis, cns]   # pick only basis = W
  # head(trenddata, 2)

  # table(trenddata$Species)
  # table(excel_ids$LATIN_NAME)
  df <- data.frame(  
    Species = c("GADU MOR", "MYTI EDU", "NUCE LAP"),
    LATIN_NAME = c("Gadus morhua", "Mytilus edulis", "Nucella lapillus"),
    stringsAsFactors = FALSE
    )
  trenddata <- left_join(trenddata, df,  by = "Species")

  # table(trenddata$Tissue)
  # table(excel_ids$TISSUE_NAME)
  df <- data.frame(
    Tissue = c("BI", "BL", "LI", "MU", "SB"),
    TISSUE_NAME = c("Bile", "Bladder", "Lever", "Muskel", "Whole soft body"),
    stringsAsFactors = FALSE
    )
  trenddata <- left_join(trenddata, df,  by = "Tissue")
  trenddata
  }

# As 'get_trends_previous_year' but picks all types of basis (W, D and F), and changes Basis to WW;DW and FB
# Also includes columns "Ant.prøver.2015", "SD.2015", "EAC.2015", "EQS.2015", as well as "DETLIM_2015"
  
prepare_results_previous_year <- function(trenddata){
  cns <- c("Parameter.Code", "Species", "Tissue", "Station.Code", "Basis",
    "Ant.prøver.2015", "SD.2015", "Klasse.2015", "EAC.2015", "EQS.2015",
    "Trend.p.long.", "Detectable...change.long.", "First.Year.long.", "Last.Year.long.", 
    "No.of.Years.long.", "Trend.p.short.", "Detectable...change.short.", 
    "First.Year.short.", "Last.Year.short.", "No.of.Years.short.", "Trends.2015",
    "DETLIM_2015")
  trenddata <- trenddata[, cns]   # pick only basis = W
  # head(trenddata, 2)

  # table(trenddata$Species)
  # table(excel_ids$LATIN_NAME)
  df <- data.frame(  
    Species = c("GADU MOR", "MYTI EDU", "NUCE LAP"),
    LATIN_NAME = c("Gadus morhua", "Mytilus edulis", "Nucella lapillus"),
    stringsAsFactors = FALSE
    )
  trenddata <- left_join(trenddata, df,  by = "Species")

  # table(trenddata$Tissue)
  # table(excel_ids$TISSUE_NAME)
  df <- data.frame(
    Tissue = c("BI", "BL", "LI", "MU", "SB"),
    TISSUE_NAME = c("Bile", "Bladder", "Lever", "Muskel", "Whole soft body"),
    stringsAsFactors = FALSE
    )
  trenddata <- left_join(trenddata, df,  by = "Tissue")

  # table(trenddata$Tissue)
  # table(excel_ids$TISSUE_NAME)
  df <- data.frame(
    Basis = c("W", "D", "F"),
    Basis_new = c("WW", "DW", "FB"),
    stringsAsFactors = FALSE
    )
  trenddata <- left_join(trenddata, df,  by = "Basis")
  
  trenddata$Basis <- trenddata$Basis_new
  trenddata$Basis_new <- NULL

  colnames(trenddata)[colnames(trenddata) %in% "Parameter.Code"] <- "PARAM"
  colnames(trenddata)[colnames(trenddata) %in% "Station.Code"] <- "STATION_CODE"

  trenddata
  }


####################################################################################################
#
# Prepare df of trends from previous year (II)
#
####################################################################################################

change_colnames_trends_previous_year <- function(trenddata){
  # Change column names
  # dput(varnames_2016)
  # dput(colnames(trend_2015_for_excel))
  cns <- c("Trend.p.long.", "Detectable...change.long.", 
    "First.Year.long.", "Last.Year.long.", "No.of.Years.long.", "Trend.p.short.", 
    "Detectable...change.short.", "First.Year.short.", "Last.Year.short.", 
    "No.of.Years.short.", "Trends.2015")
  cns_new <- c("Trend p(long)2015", "Detectable % change(long)2015", 
    "First Year(long)2015", "Last Year(long)2015", "No of Years(long)2015", 
    "Trend p(short)2015", "Detectable % change(short)2015", "First Year(short)2015", 
    "Last Year(short)2015", "No of Years(short)2015", "Trends 2015")
  # change names
  cn_change <- data.frame(old = cns, new = cns_new, stringsAsFactors = FALSE)
  for (i in 1:nrow(cn_change)){
    sel <- colnames(trenddata) %in% cn_change[i,"old"]
    colnames(trenddata)[sel] <- cn_change[i,"new"]
    }
  trenddata
  }

  
####################################################################################################
#
# Prepare df of trends from previous year (III)
# Combines functions I and II
#
####################################################################################################


make_trend_data_for_excel_previousyear <- function(trend_data_prev, excel_ids, basis){
  trend_data <- get_trends_previous_year(trend_data_prev, basis = basis)
  # Put the trend data in the same order as the Excel file with background/level data
  trenddata_for_excel <-  left_join(excel_ids, trend_data,  
    by = c("PARAM" = "Parameter.Code", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE" = "Station.Code")) %>%
    as.data.frame()
  # Define some columns
  trenddata_for_excel2 <- change_colnames_trends_previous_year(trenddata_for_excel)
  trenddata_for_excel2
  }


#
# As above but also joins using Basis
#
make_trend_data_for_excel_previousyear <- function(trend_data_prev, excel_ids){
  trend_data <- get_trends_previous_year(trend_data_prev, basis = basis)
  # Put the trend data in the same order as the Excel file with background/level data
  trenddata_for_excel <-  left_join(excel_ids, trend_data,  
    by = c("PARAM" = "Parameter.Code", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE" = "Station.Code")) %>%
    as.data.frame()
  # Define some columns
  trenddata_for_excel2 <- change_colnames_trends_previous_year(trenddata_for_excel)
  trenddata_for_excel2
  }


