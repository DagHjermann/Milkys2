
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Various utility functions ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

#
# List files in 'folder' containing 'pattern'
# - 'pattern' works as a normal regular expression (see ?regexp) 
#   Example: pattern = "Result.*" will return all files starting with "Result"
#      ("." means "any character", ".*" means "any number of characters")
# - exception: it accepts "[:date:]" as part of 'pattern', which is interpreted as date
#   in the format "2019-12-24"
# - 'extension' is the file extension
# - The function also accepts other arguments of dir() such as
#   ignore.case = FALSE/TRUE (default = FALSE)
#   include.dirs = FALSE/TRUE (default = FALSE)
#   See ?dir for more
#   

list_files <- function(folder, pattern, extension = NULL, ...){
  pattern2 <- sub(
    "[:date:]", 
    "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]",
    pattern, fixed = TRUE)
  if (!is.null(extension)){
    extension <- sub(".", "", extension, fixed = TRUE)
    pattern2 <- paste0(pattern2, "\\.", extension, ...)
  }
  files <- dir(folder, pattern2) %>% rev()
  cat("There are", length(files), "files with pattern", sQuote(pattern))
  if (!is.null(extension))
    cat(" and extension", sQuote(extension))
  cat(" to choose from \n")
  files
}

# Tests
if (FALSE){
  # debugonce(list_files)
  list_files("Data", "120_result_10yr.*")
  list_files("Data", "120_result_10yr_[:date:]", ".rds")
  list_files("Data", "120_result_10yr_[:date:]", "rds")
}


# Reading R data (rds) file - including the given date in the file name 
# Input: 
#  folder     = name of folder
#  files      = character string of file names (without folder name)
#  filenumber = which file number (among 'files') we will read
#  get_date   = do you want to extract the date from the file name?
#  time_since_modified = do you want to print how long time has passed since the file was modified?
#  
# If get_date = FALSE (the default), the function returns just the data
# If get_date = TRUE, the function returns a list consisiting of the data itself ($data) and the date ($file_date)
# 
read_rds_file <- function(folder, files, filenumber, get_date = FALSE, time_since_modified = FALSE){
  # Get file name
  filename <- files[filenumber] 
  # Read data
  data <- readRDS(paste0(folder, "/", filename))  
  cat("File ", sQuote(filename), " (file number ", filenumber, ") has been read \n", sep = "")
  # Give a warning that there are more files that can be used (if so is true)
  if (filenumber == 1 & length(files) > 1){
    cat("  This is the newest file. If you want to read an older file, put a different 'filenumber' \n")
  }
  # Print how old the file is
  if (time_since_modified){
    cat("\nTime since this file was modified: \n")
    print(Sys.time() - file.info(paste0(folder, "/", filename))$mtime)
  }
  # If you also want to extract the date from the file name 
  if (get_date){
    # Get the date part of the text (e.g., '2020-04-23')
    # This can be used later in the script, when we save the resulting file
    pos1 <- regexpr("20[0-9][0-9]\\-[0-9][0-9]\\-[0-9][0-9]", filename)   # search for the first place a date with this format occurs inside filename
    #   (Assuming the the date is sometime in the 2000s)
    if (pos1 > 0){
      pos2 <- pos1 + 9                            # Assuming format like '2020-04-23'  
      file_date <- substr(filename, pos1, pos2)   # Pick out the part of the text from character no. 17 to no. 26
    } else {
      file_date <- NA
    }
    result <- list(data = data, file_date = file_date)  # we return a list
  } else {
    result = data   # we return just the data
  }
  result
}

# Example
if (FALSE){
  files = c("109_adjusted_data_2020-08-05.rds", "109_adjusted_data_2020-07-03.rds", 
            "109_adjusted_data_2020-07-03.rds", "109_adjusted_data_2020-06-12.rds", 
            "109_adjusted_data_2020-05-29.rds")
  X <- read_rds_file("Data",
                     files, filenumber = 5,   # gets the file from May 2020
                     get_date = TRUE, time_since_modified = TRUE)
  the_data_itself <- X$data  # get data
  X$file_date                # "2020-05-29"
  
}

#
# Change trend symbols used in Excel into actual trend symbols that can be read in R
#   (arrows etc.) 
#

symbol_from_text <- function(txt){
  # Note tha not all encodings found in unicode-search.net works. 
  # For instance, "\u1F805" should be "upwards arrow with medium triangle arrowhead", but isn't
  result <- gsub("é", "\u2191", txt)     # arrow up
  result <- gsub("ê", "\u2193", result)  # arrow down
  result <- gsub("¢", "\u25CB", result)  # circle
  result <- gsub("§", "\u2605", result)  # star
  gsub("«", "\u25A2", result)            # square
}

if (FALSE){
  # Reproducible example
  readRDS("Big_excel_table/Data_xl_2020-08-05_ver15.rds") %>%
    filter(PARAM == "HG" & TISSUE_NAME == "Muscle" & Basis == "WW") %>%
    .[c("PARAM", "LATIN_NAME", "TISSUE_NAME", "STATION_CODE", "Basis", "Trends.2019")] %>%
    mutate(Trends.2019 = symbol_from_text(Trends.2019)) %>%
    View()
}

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


#
# "Round" p-values (returns text)
#
round_p <- function(x, stars = FALSE){
  text <- case_when(
    x >= 0.2 ~ sprintf("%.1f", x),
    x >= 0.06 ~ sprintf("%.2f", x),
    x >= 0.001 ~ sprintf("%.3f", x),
    x >= 0.0001 ~ sprintf("%.4f", x),
    x < 0.0001 ~ "<0.0001"
  )
  if (stars){
    text <- case_when(
      text <= 0.001 ~ paste(text, "***"),
      text <= 0.01 ~ paste(text, "**"),
      text <= 0.05 ~ paste(text, "*"),
      text <= 0.1 ~ paste(text, "(*)"),
      text > 0.1 ~ text
    ) 
  }
  text
}

# TEST
if (FALSE){
  round_p(0.121232)
  round_p(0.0121232)
  round_p(0.00121232)
  round_p(0.000121232)
  round_p(0.121232, stars = TRUE)
  round_p(0.0121232, stars = TRUE)
  round_p(0.00121232, stars = TRUE)
  round_p(0.000121232, stars = TRUE)
}


#
# Function for checking that all values of a variable are numeric
# - 'variable' is given as a text (in quotes) 
# - na_allowed = TRUE: NA values are 'allowed'; all values must be numeric or NAs
# - na_allowed = FALSE: NA values are not 'allowed'; all values must be numeric
#
check_variable_is_numeric <- function(data, variable, na_allowed = TRUE){
  old.options <- options(warn = -1)
  if (na_allowed){
    # not NA but not numeric
    sel <- is.na(as.numeric(data[[variable]])) & !is.na(data[[variable]])  
  } else {
    # all that are not numeric or NA
    sel <- is.na(as.numeric(data[[variable]]))  
  }
  data_problems <- data[sel,]
  options(old.options)
  if (na_allowed){
    if (nrow(data_problems) > 0){
      warning("Some values of ", variable, " have non-numeric values! Inspect result.")
    } else {  
      message("All values of ", variable, " are numeric or NA")
    }
  } else {
    if (nrow(data_problems) > 0){
      warning("Some values of ", variable, " are empty or have non-numeric values! Inspect result.")
    } else {  
      message("All values of ", variable, " are numeric")
    }
  }
  invisible(data_problems)
}




#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Downloading ICES data ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# get_ices_biotadata
# New version (Jan 2020) of 'get_ices_data' for getting data (biota data only)
#
# 1. Using the following web service: http://dome.ices.dk/Webservices/index.aspx
# 2. Using url2 (which reads the data as UTF-8) insead of package XML
# 3. Much more effective code (using package purrr)
#
# Needs only PARAM to be set
#
# For other parameters, see examples below
# Note: don't know what it wants for "species"
#   (also tried code from http://ecosystemdata.ices.dk/webservices/EcoSystemWebServices.asmx/getListSpecies )
# 

# Country codes: https://vocab.ices.dk/?ref=22 
#   NOTE: downloaded to "00_read_ICES_webservice_functions_countries.csv"
# Matrix codes: MU, LI, SB etc. 
# Parameter groups (paramgroup): https://vocab.ices.dk/?ref=78 

# Example:
# All PYR1OH data from Norway  
# df1 <- get_ices_biotadata(param = "PYR1OH", country = 58)


get_ices_biotadata <- function(param, yearstart = NULL, yearend = yearstart, 
                               country = "", matrix = "", lab = "", species = "",
                               paramgroup = ""){
  if (is.null(yearstart)){
    yearstart <- ""
  } else {
    yearstart <- as.character(yearstart)
  }
  if (is.null(yearend)){
    yearend <- ""
  } else {
    yearend <- as.character(yearend)
  }
  url_part1 <- "http://dome.ices.dk/Webservices/DOMEWebServices.asmx/selectContaminantsInBiota?"
  url_part2_txt <- "PARAM=%s&RLABO=%s&ALABO=&yearBegining=%s&yearEnd=%s&MATRX=%s&TAXA=%s&PURPM=&MPROG=&Area=&CNTRY=%s&ParamGroup=%s"
  url_part2 <- sprintf(url_part2_txt, 
                       param, lab, yearstart, yearend, matrix, species, country, paramgroup)
  xml_url <- paste0(url_part1, url_part2)
  cat("Querying dome.ices.dk using the following URL:\n")
  cat(xml_url)
  cat("\n")
  # OLD (using package XML)
  # xmlfile <- xmlTreeParse(xml_url, encoding = "UTF-8")
  # xmltop = xmlRoot(xmlfile)
  # list_of_vectors <- xmlSApply(xmltop, function(x) xmlSApply(x, xmlValue, encoding = "UTF-8"))
  # list_of_dataframes <- purrr::map(list_of_vectors, vector_to_dataframe)
  # NEW (using package xml2)
  df <- xml_to_dataframe(xml_url)
  if (!is.null(df)){
    numeric_vars <- c("MYEAR", "Latitude", "Longitude", "Value", "UNCRT", 
                      "SUBNO", "BULKID", 
                      "tblAnalysisID", "tblParamID", "tblBioID", "tblSampleID")
    for (var in numeric_vars){
      if(var %in% names(df))
        df[[var]] <- as.numeric(df[[var]])
    }
    df$DATE <- lubridate::dmy(df$DATE)
  }
  df
}

if (FALSE){
  # debugonce(get_ices_biotadata)
  X <- get_ices_biotadata("HG", yearstart = 2018, lab = "NIVA")   # 371 lines
  X <- get_ices_biotadata("HG", yearstart = 2018, country = 58)    # 371 lines (58 = Norway; see https://vocab.ices.dk/?ref=22)
  X <- get_ices_biotadata("HG", yearstart = 2018)   # 1309 lines (all countries)
  X <- get_ices_biotadata("HG", yearstart = 2018, lab = "NIVA")   # 371 lines
  X <- get_ices_biotadata("HG", yearstart = 2018, lab = "NIVA", species = "Gadus morhua")     # doesn't work
  X <- get_ices_biotadata("HG", yearstart = 2018, lab = "NIVA", species = "gadus morhua")     # doesn't work
  X <- get_ices_biotadata("HG", yearstart = 2018, lab = "NIVA", species = "Gadus%20morhua")   # doesn't work
  X <- get_ices_biotadata("HG", yearstart = 2018, lab = "NIVA", species = "5558")  # doesn't work either (code from http://ecosystemdata.ices.dk/webservices/EcoSystemWebServices.asmx/getListSpecies)
  X <- get_ices_biotadata("HG", yearstart = 2018, lab = "NIVA", matrix = "MU")   # 251 lines
  X <- get_ices_biotadata("HG", yearstart = 2009, yearend = 2010, lab = "NIVA", matrix = "MU")   # 251 lines
  # Error handling is not too good:
  X <- get_ices_biotadata("Bogus", yearstart = 2018, lab = "NIVA")
  X <- get_ices_biotadata("", paramgroup = "O-MET", yearstart = 2018, lab = "NIVA", matrix = "SB")   # all organic metals (TBT etc)
}


get_ices_sedimentdata <- function(param, yearstart = NULL, yearend = yearstart, 
                                  country = "", lab = "", 
                                  paramgroup = ""){
  if (is.null(yearstart)){
    yearstart <- ""
  } else {
    yearstart <- as.character(yearstart)
  }
  if (is.null(yearend)){
    yearend <- ""
  } else {
    yearend <- as.character(yearend)
  }
  # url_part1 <- "http://dome.ices.dk/Webservices/DOMEWebServices.asmx/selectContaminantsInBiota?"
  url_part1 <- "http://dome.ices.dk/Webservices/DOMEWebServices.asmx/selectContaminantsInSediment?"
  # "PARAM=CD&RLABO=ALUK&ALABO=&yearBegining=&yearEnd=&MATRX=&TAXA=&PURPM=&MPROG=&Area=&CNTRY=&ParamGroup="
  url_part2_txt <- "PARAM=%s&RLABO=%s&ALABO=&yearBegining=%s&yearEnd=%s&MATRX=&TAXA=&PURPM=&MPROG=&Area=&CNTRY=%s&ParamGroup=%s"
  url_part2 <- sprintf(url_part2_txt, 
                       param, lab, yearstart, yearend, country, paramgroup)
  xml_url <- paste0(url_part1, url_part2)
  cat("Querying dome.ices.dk using the following URL:\n")
  cat(xml_url)
  cat("\n")
  # OLD (using package XML)
  # xmlfile <- xmlTreeParse(xml_url, encoding = "UTF-8")
  # xmltop = xmlRoot(xmlfile)
  # list_of_vectors <- xmlSApply(xmltop, function(x) xmlSApply(x, xmlValue, encoding = "UTF-8"))
  # list_of_dataframes <- purrr::map(list_of_vectors, vector_to_dataframe)
  # NEW (using package xml2)
  df <- xml_to_dataframe(xml_url)
  if (!is.null(df)){
    numeric_vars <- c("MYEAR", "Latitude", "Longitude", "Value", "UNCRT", 
                      "SUBNO", "BULKID", 
                      "tblAnalysisID", "tblParamID", "tblBioID", "tblSampleID")
    for (var in numeric_vars){
      if(var %in% names(df))
        df[[var]] <- as.numeric(df[[var]])
    }
    df$DATE <- lubridate::dmy(df$DATE)
  }
  df
}
# Test
# X <- get_ices_sedimentdata("CD", yearstart = 1996, country = 74)    # 68 lines (74 = UK; see https://vocab.ices.dk/?ref=22)


if (FALSE){
  # debugonce(get_ices_biotadata)
  X <- get_ices_biotadata("HG", yearstart = 2018, lab = "NIVA")   # 371 lines
  X <- get_ices_biotadata("HG", yearstart = 2018, country = 58)    # 371 lines (58 = Norway; see https://vocab.ices.dk/?ref=22)
  X <- get_ices_biotadata("HG", yearstart = 2018)   # 1309 lines (all countries)
  X <- get_ices_biotadata("HG", yearstart = 2018, lab = "NIVA")   # 371 lines
  X <- get_ices_biotadata("HG", yearstart = 2018, lab = "NIVA", species = "Gadus morhua")     # doesn't work
  X <- get_ices_biotadata("HG", yearstart = 2018, lab = "NIVA", species = "gadus morhua")     # doesn't work
  X <- get_ices_biotadata("HG", yearstart = 2018, lab = "NIVA", species = "Gadus%20morhua")   # doesn't work
  X <- get_ices_biotadata("HG", yearstart = 2018, lab = "NIVA", species = "5558")  # doesn't work either (code from http://ecosystemdata.ices.dk/webservices/EcoSystemWebServices.asmx/getListSpecies)
  X <- get_ices_biotadata("HG", yearstart = 2018, lab = "NIVA", matrix = "MU")   # 251 lines
  X <- get_ices_biotadata("HG", yearstart = 2009, yearend = 2010, lab = "NIVA", matrix = "MU")   # 251 lines
  # Error handling is not too good:
  X <- get_ices_biotadata("Bogus", yearstart = 2018, lab = "NIVA")
  X <- get_ices_biotadata("", paramgroup = "O-MET", yearstart = 2018, lab = "NIVA", matrix = "SB")   # all organic metals (TBT etc)
}

xmlchild_to_dataframe <- function(i, xmlchildren){
  grandchildren <- xml2::xml_children(xmlchildren[[i]])
  df <- data.frame(matrix(xml2::xml_text(grandchildren), nrow = 1), stringsAsFactors = FALSE)
  colnames(df) <- xml2::xml_name(grandchildren)
  df
}

xml_to_dataframe <- function(url){
  full_xml <- xml2::read_xml(url)
  children <- xml2::xml_children(full_xml)
  if (length(children) > 0){
    list_of_dataframes <- seq_along(children) %>% purrr::map(~xmlchild_to_dataframe(., children))
    df <- dplyr::bind_rows(list_of_dataframes)
  } else {
    cat("No results for this database query\n")
    df <- NULL
  }
  df
}


