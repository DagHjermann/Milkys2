
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
# Round to number of significant values, modified
# - as the base function signif(), but
#     - never returns scientific rotation     
#     - drops zero for numbers under 1, e.g. signif2(0.1234, 2)  
#     - never rounds numbers to more than integer, compare signif(123) and signif2(123)   
#     - sets maximum number of digits
# - note the function has been vectorized
# - does NOT round numbers < 0
#
#

signif2 <- function(x, digits, dropzero = TRUE, maxdigits = 8){
  sel <- !is.na(x) & x > 0
  digits2 <- pmax(digits-ceiling(log10(x[sel])), 0)
  digits2 <- pmin(digits2, maxdigits)
  # round(x, digits2)
  result_sel <- sprintf("%.*f", digits2, x[sel])
  if (dropzero){
    result_sel <- ifelse(x[sel] < 1, 
                         substr(result_sel, 2, nchar(result_sel)),
                         result_sel
    )
  }
  result <- x
  result[sel] <- result_sel
  result
}

if (FALSE){
  # debugonce(signif2)
  signif2(0.1234, 2)
  signif2(0.1234, 2)
  signif2(0.1234, 2, dropzero = FALSE)
  signif2(0.01234, 2)
  signif2(0.001234, 2)
  signif2(0.0001234, 2)
  signif2(0.0001234, 2, maxdigits = 4)
  signif2(0.000001234, 2)    # no scientific notation  
  signif(0.000001234, 2)     # scientific notation, unless you have changed 'scipen' in options()
  signif2(c(1.234,0.1234,0.01234,0.001234,0.0001234), 2, maxdigits = 4)  # check that it has been vectorized
  signif2(c(1.234,NA,NA,0.001234,0.0001234), 2, maxdigits = 4)  # check that it works for NAs
  signif2(c(1.234,0.1234,0.01234,-1.234,-0.1234,-0.01234), 2, maxdigits = 4)  # Note: under-zero are not rounded!
  signif2(1.234, 2)
  debugonce(signif2)
  signif2(12.34, 2)
  signif2(123.4, 2)
  signif(123.4, 2)   # signif, for comparison
  signif2(c(12.34, 123.4, 1234), 2)  # check that it has been vectorized
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


#
# Check if we use the latest file 
# - where the file name conforms to a pattern  
# - note: assumes that files are sorted by time when using sort(). 
#   I.e., that date or time is in the form YYYY-MM-DD or some other sortable form (e.g.YYYY-MM-DD HH-MM)
#   and that everyting after the dat/time (usually, just the extension) is always the same  
#

check_latest <- function(filename, folder, pattern, 
                         namelength = 0,                              # filter by namelength (can also be number > 0) 
                         reaction = "error", 
                         ok_message = FALSE){
  
  if (!grepl(pattern, filename)){
    message <- paste0("Given file (", sQuote(filename), ") doesn't correspond to pattern given (", sQuote(pattern), ")")
    stop(message)
  }
  
  filenames_found <- dir(folder, pattern = pattern)
  
  if (length(filenames_found) == 0){
    message <- paste0("no files with given 'pattern' (", sQuote(pattern), ") found in 'folder' (", sQuote(folder), ")")
    stop(message)
  }
  
  if (!is.null(namelength) & namelength >= 0){
    # namelength= 0 means that we take namelength from 'filename'  
    if (namelength == 0)
      namelength <- nchar(filename)
    filenames_found <- filenames_found[nchar(filenames_found) == namelength]
  }
  
  filename_latest <- sort(filenames_found) %>% tail(1)
  
  if (filename_latest != filename){
    if (!filename %in% filenames_found){
      stop("Given file (", sQuote(filename), ") not found in ", sQuote(folder), "\n",
           "The latest file with the given pattern is ", sQuote(filename_latest))
    }
    message <- paste0(
      "The latest file seems not to be used. The latest file in ", sQuote(folder), " with pattern ", sQuote(pattern),
      " is ", sQuote(filename_latest))
    if (reaction == "warning"){
      warning(message)
    } else if (reaction == "message"){
      message(message)
    } else {
      stop(message)
    }
    
  } else if (ok_message) {
    message <- paste0(
      "The latest file is used (folder ", sQuote(folder), ", pattern ", sQuote(pattern), ")")
    message(message)
  }
  
  invisible(NULL)
  
}

#
# test
if (FALSE){
  
  debugonce(check_latest)
  
  # Latest file given  
  fn <- "109_adjusted_data_2022-09-23.rds"
  check_latest(fn, "Data", "109_adjusted_data_")
  check_latest(fn, "Data", "109_adjusted_data_", ok_message = TRUE)
  
  # Latest file not given  
  fn <- "109_adjusted_data_2022-09-01.rds"
  check_latest(fn, "Data", "109_adjusted_data_")
  check_latest(fn, "Data", "109_adjusted_data_", reaction = "warning")
  check_latest(fn, "Data", "109_adjusted_data_", reaction = "message")

  # Wrong pattern given  
  fn <- "110_mediandata_updated_2022-09-23.rds"
  check_latest(fn, "Data", "109_adjusted_data_")
  
  # File doesn't exist   
  fn <- "109_adjusted_data_2022-09-41.rds"
  check_latest(fn, "Data", "109_adjusted_data_")
  
  # With function  
  test_get_medians <- function(folder, file){
    check_latest(file, folder, "110_mediandata_updated_")
    readRDS(paste0(folder, "/", file))
  }
  test <- test_get_medians("Data", "110_mediandata_updated_2022-09-23.rds")
  test <- test_get_medians("Data", "110_mediandata_updated_2022-09-01.rds")
  test <- test_get_medians("Data", "110_mediandata_updated_2022-09-24.rds")
  
}


#
# update part of a data frame using func()
#

# Example:
if (FALSE){
  
update_part(
  df_in = dat_concentrations, 
  df_out = dat_trends, 
  var_list = list(
    LATIN_NAME = "Gadus morhua",
    PARAM = c("CD", "PB", "HG"),
    Basis = "WWa"
  ), 
  func = calculate_trends,
  add = TRUE      # If TRUE, rows not existing in df_out will be added
)                 # If FALSE, there will be a warning if there are rows not existing in df_out  
}

update_part <- function(df_in, df_out, var_list, func, add = TRUE){
  # NOT WRITTEN YET
}

# Returns string with "unoccupied" filen name for backup. See examples below
get_backup_filename <- function(folder, filename_without_extension, extension = "xlsx", backup_prefix = "_OLD"){
  string_for_pattern <- paste0(filename_without_extension, backup_prefix, ".*.", extension)
  fns <- dir(folder, pattern = string_for_pattern) 
  if (length(fns) == 0){
    fn_back <- paste0(folder, "/", filename_without_extension, backup_prefix, "1.", extension)
  } else {
    string_for_extract <- paste0("(?<=", backup_prefix, ")[^", backup_prefix, "]+(?=\\.)")
    version_no <- as.integer(stringr::str_extract(fns, "(?<=_OLD)[^_OLD]+(?=\\.)"))
    version_no_new <- max(version_no, na.rm = TRUE) + 1
    fn_back <- paste0("Lookup table - preferred parameter units_OLD", version_no_new, ".xlsx")
    fn_back <- paste0(folder, "/", filename_without_extension, backup_prefix, version_no_new, ".", extension)
  }
  fn_back
}
# Examples:
# get_backup_filename("Input_data", "Lookup table - preferred parameter units")
# - if 'Lookup table - preferred parameter units_OLD6.xlsx' exists, this should return 
#   'Lookup table - preferred parameter units_OLD7.xlsx'
# - if no files marked 'OLD' exists, this should return 
#   'Lookup table - preferred parameter units_OLD1.xlsx'
# get_backup_filename("Input_data", "Lookup table - preferred parameter units", extension = "csv")

write_csv_with_backup <- function(data, folder, filename_without_extension, extension = "csv", backup_prefix = "_OLD"){
  fn <- paste0(folder, "/", filename_without_extension, ".csv")
  fn_back <- get_backup_filename(
    folder = folder, 
    filename_without_extension = filename_without_extension,
    extension = extension,
    backup_prefix = backup_prefix)
  if (!file.exists(fn_back)){
    file.copy(fn, fn_back)
    write_csv(data, fn)
  } else {
    stop(fn_back, " exists!")  # this shouldn't theoretically happen
  }
}
# Example:
#   write_csv_with_backup(data, "Input_data", "Lookup table - preferred parameter units", extension = "csv")
# Takes a backup of "Lookup table - preferred parameter units.csv" with an unocccupied (numbered) backup name,
#   e.g. "Lookup table - preferred parameter units_OLD4.csv"
# Then overwrites "Lookup table - preferred parameter units.csv" with the data 
  

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


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Functions for creating SQL code ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


make_sql_single_specimen <- function(i, data){
  
  df <- as.data.frame(data)
  
  original_options <- options(useFancyQuotes = FALSE)
  txt <- paste0("insert into NIVADATABASE.BIOTA_SINGLE_SPECIMENS ",
                "(DATE_CAUGHT, STATION_ID, TAXONOMY_CODE_ID, SPECIMEN_NO)\n",  # \n for line shift
                "values (",
                "TO_DATE(", sQuote(df[i, 'DATE_CAUGHT']), ", 'YYYY-MM-DD'), ",
                df[i, 'STATION_ID'], ", ",
                df[i, 'TAXONOMY_CODE_ID'], ", ",
                df[i, 'SPECIMEN_NO'],
                ")"
  )
  options(original_options)
  txt
}

# make_sql_single_specimen(1, biota_single_specimens_eider)
# make_sql_single_specimen(2, biota_single_specimens_eider)



make_sql_sample <- function(i, data){
  
  df <- as.data.frame(data)
  
  original_options <- options(useFancyQuotes = FALSE)
  txt <- paste0("insert into NIVADATABASE.BIOTA_SAMPLES ",
                "(STATION_ID, TISSUE_ID, REPNO, TAXONOMY_CODE_ID, SAMPLE_DATE, SAMPLE_NO)\n",  # \n for line shift
                "values (",
                df[i, 'STATION_ID'], ", ",
                df[i, 'TISSUE_ID'], ", ",
                df[i, 'REPNO'], ", ",
                df[i, 'TAXONOMY_CODE_ID'], ", ",
                "TO_DATE(", sQuote(df[i, 'SAMPLE_DATE']), ", 'YYYY-MM-DD'), ",
                df[i, 'SAMPLE_NO'],
                ")"
  )
  options(original_options)
  txt
}


make_sql_samples_specimens <- function(i, data){
  
  df <- as.data.frame(data)
  
  original_options <- options(useFancyQuotes = FALSE)
  txt <- paste0("insert into NIVADATABASE.BIOTA_SAMPLES_SPECIMENS ",
                "(SAMPLE_ID, SPECIMEN_ID)\n",  # \n for line shift
                "values (",
                df[i, 'SAMPLE_ID'], ", ",
                df[i, 'SPECIMEN_ID'],
                ")"
  )
  options(original_options)
  txt
}

# Test
# make_sql_samples_specimens(1, biota_sample_specimens_eider)


#
# BIOTA_CHEMISTRY_VALUES
#

# "VALUE_ID"              - Let the database decide
# "SAMPLE_ID"             - From the database, after BIOTA_SAMPLES have been inserted
# "METHOD_ID"             - Lookup based on NAME and UNIT
# "VALUE"                 - From data
# "FLAG1"                 - From data
# "FLAG2"                 - NA
# "ENTERED_BY"            - DHJ
# "ENTERED_DATE"          - date, see above
# "REMARK"                - NA
# "DETECTION_LIMIT"       - NA
# "UNCERTAINTY"           - NA
# "QUANTIFICATION_LIMIT"  - NA
# "APPROVED"              - NA?


make_sql_chemistry_values <- function(i, data){
  
  df <- as.data.frame(data)
  
  original_options <- options(useFancyQuotes = FALSE)
  
  flag <- df[i, 'FLAG1']
  txt <- paste0("insert into NIVADATABASE.BIOTA_CHEMISTRY_VALUES ",
                "(SAMPLE_ID, METHOD_ID, VALUE, FLAG1, APPROVED)\n",  # \n for line shift
                "values (",
                df[i, 'SAMPLE_ID'], ", ",
                df[i, 'METHOD_ID'], ", ",
                round(df[i, 'VALUE'], 6), ", ",
                ifelse(is.na(flag), "NULL", sQuote(flag)), ", ",
                1,
                ")"
  )
  options(original_options)
  txt
}
# Test
# make_sql_chemistry_values(1, biota_chemistry_values_eider)

#
# For "select all" - NOT FINISHED!
#
# make_sql_chemistry_values_intoall <- function(lines, data){
#   
#   df <- as.data.frame(data)
#   data_section <- make_sql_chemistry_values_single <- function( data)
#   
#   original_options <- options(useFancyQuotes = FALSE)
#   txt <- paste0("insert all\n",
#                 data_section,
#                 "select 1 from dual"
#   )
#   options(original_options)
#   txt
# }

#
# For "select all" - NOT FINISHED!
#

# make_sql_chemistry_values_single <- function(i, data){
#   
#   df <- as.data.frame(data)
#   
#   original_options <- options(useFancyQuotes = FALSE)
#   
#   flag <- round(df[i, 'FLAG1'], 6)
#   txt <- paste0("    into NIVADATABASE.BIOTA_CHEMISTRY_VALUES ",
#                 "    (SAMPLE_ID, METHOD_ID, VALUE, FLAG1,APPROVED)\n",  # \n for line shift
#                 "    values (",
#                 df[i, 'SAMPLE_ID'], ", ",
#                 df[i, 'METHOD_ID'], ", ",
#                 round(df[i, 'VALUE'], 6), ", ",
#                 ifelse(is.na(flag), "NULL", sQuote(flag)),
#                 1,
#                 ")"
#   )
#   options(original_options)
#   txt
# }



make_sql_methods <- function(i, data){
  
  df <- as.data.frame(data)
  
  original_options <- options(useFancyQuotes = FALSE)
  
  name <- df[i, 'NAME']
  unit <- df[i, 'UNIT']
  lab <- df[i, 'LABORATORY']
  method_ref <- df[i, 'METHOD_REF']
  matrix <- df[i, 'MATRIX']
  cas <- df[i, 'CAS']
  txt <- paste0(
    "insert into NIVADATABASE.METHOD_DEFINITIONS ",
    "(NAME, UNIT, LABORATORY, METHOD_REF, MATRIX, CAS, MATRIX_ID)\n",  # \n for line shift
    "values (",
    ifelse(is.na(name), "NULL", sQuote(name)), ", ",
    ifelse(is.na(unit), "NULL", sQuote(unit)), ", ",
    ifelse(is.na(lab), "NULL", sQuote(lab)), ", ",
    ifelse(is.na(method_ref), "NULL", sQuote(method_ref)), ", ",
    ifelse(is.na(matrix), "NULL", sQuote(matrix)), ", ",
    ifelse(is.na(cas), "NULL", sQuote(cas)), ", ",
    df[i, 'MATRIX_ID'],
    ")"
  )
  options(original_options)
  txt
}

# See script 75:
# make_sql_methods(1, new_methods)


#
# Helper functions for making SQL parts
#
# Takes the unique values of a variable and puts them in a bracket (sql style)
# 
#

make_sql_ids <- function(data, variable){
  values <- data[[variable]] %>% unique()
  if (class(data[[variable]]) == "character"){
    original_options <- options(useFancyQuotes = FALSE)
    values <- sQuote(values)
    options(original_options)
  }
  paste0("(",
         values %>% paste(collapse = ","),
         ")")
}

# make_sql_ids(biota_samples, "STATION_ID")      
# "(46980,47221,50478,67807,69711)"
#
# make_sql_ids(biota_chemistry_values, "FLAG1")
# "('<','NA')"



#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Functions for reading data from Nivabase  ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


test_connection <- function(connection){
  
  # Test a small download
  test_connection <- DBI::dbGetQuery(connection, "select PROJECT_ID, PROJECT_NAME from NIVADATABASE.PROJECTS where rownum < 3")
  
  # Check that download worked
  if ("test_connection" %in% ls()){
    if (nrow(test_connection) == 2)                                                  
      cat("Connection to Nivadatabase set up and tested")
  } else {
    warning("Test download failed")
  }
  
}

get_connection <- function(test_only = FALSE){
  
  # Set up connection (asks for user name and password)
  connection <- DBI::dbConnect(odbc::odbc(),
                 Driver = "/opt/conda/orahome/libsqora.so.12.1",
                 DBQ = "dbora-niva-prod01.niva.corp:1555/NIVABPRD",
                 UID = rstudioapi::askForPassword("Database username (NIVA initials, 3 capital letters)"),
                 PWD = rstudioapi::askForPassword("Nivabasen password"),
                 encoding = "ISO-8859-1"
  )
  
  options(useFancyQuotes = FALSE)
  
  test_connection(connection)
  
  connection
                 
}

if (FALSE){
  # Test
  con <- get_connection()  
  DBI::dbGetQuery(con, "select * from NIVADATABASE.PROJECTS_O_NUMBERS where rownum < 3")
  library(dplyr)
  library(dbplyr)
  projects %>%
    filter(substr(PROJECT_NAME,1,4) %in% "CEMP") %>%
    collect()
}


define_nivabasen_tables <- function(connection){
  
  require(dbplyr)
  
  object_names <- c("t_lims_id", "t_measurements", "t_methods", "t_basis", "t_project_stations", 
                    "t_projects", "t_projects_onumbers", "t_samples", "t_samples_specimens", 
                    "t_specimens", "t_taxonomy", "t_taxonomy_codes", "t_tissue")
  # Find existing variables in the global environment ("n = 1") starting with "t_"
  existing_object_names <- ls(name = 1, pattern = "^t_")
  overlap <- intersect(object_names, existing_object_names)
  
  create_objects <- TRUE
  if (length(overlap) > 0){
    answer <- ""
    while (!answer %in% c("y","n")){
      cat("This will overwrite the following existing objects:\n")
      print(overlap)
      answer <- readline("OK to continue? (y/n)")
      if (answer == "n"){
        create_objects <- FALSE
        break()
      }
    }
  }
  
  if (create_objects){
    
    t_methods <<- tbl(connection, in_schema("NIVADATABASE", "METHOD_DEFINITIONS")) %>%
      select(METHOD_ID, NAME, UNIT, LABORATORY, METHOD_REF, MATRIX_ID, BASIS_ID)
    t_basis <<- tbl(connection, in_schema("NIVADATABASE", "BASIS_DEFINITIONS")) %>%
      select(BASIS_ID, BASIS_CODE)
    t_measurements <<- tbl(connection, in_schema("NIVADATABASE", "BIOTA_CHEMISTRY_VALUES")) %>%
      select(SAMPLE_ID, METHOD_ID, VALUE_ID, VALUE, FLAG1, 
             DETECTION_LIMIT, UNCERTAINTY, QUANTIFICATION_LIMIT)
    # drop STATION_ID, TAXONOMY_CODE_ID from samples
    t_samples <<- tbl(connection, in_schema("NIVADATABASE", "BIOTA_SAMPLES")) %>%
      select(SAMPLE_ID, TISSUE_ID, SAMPLE_NO, REPNO, REMARK) %>%
      rename(REMARK_sample = REMARK)
    t_samples_specimens <<- tbl(connection, in_schema("NIVADATABASE", "BIOTA_SAMPLES_SPECIMENS")) %>%
      select(SPECIMEN_ID, SAMPLE_ID)
    t_specimens <<- tbl(connection, in_schema("NIVADATABASE", "BIOTA_SINGLE_SPECIMENS")) %>%
      select(STATION_ID, SPECIMEN_ID, SPECIMEN_NO, DATE_CAUGHT, TAXONOMY_CODE_ID, REMARK) %>%
      rename(REMARK_specimen = REMARK)
    t_project_stations <<- tbl(connection, in_schema("NIVADATABASE", "PROJECTS_STATIONS")) %>%
      select(STATION_ID, STATION_CODE, STATION_NAME, PROJECT_ID )
    t_projects <<- tbl(connection, in_schema("NIVADATABASE", "PROJECTS")) %>%
      select(PROJECT_ID, PROJECT_NAME, PROJECT_DESCRIPTION)
    t_projects_onumbers <<- tbl(connection, in_schema("NIVADATABASE", "PROJECTS_O_NUMBERS")) %>%
      select(PROJECT_ID, O_NUMBER)
    t_stations <<- tbl(connection, in_schema("NIVADATABASE", "STATIONS")) %>%
      select(STATION_ID, GEOM_REF_ID)
    t_coordinates <<- tbl(connection, in_schema("NIVA_GEOMETRY", "SAMPLE_POINTS")) %>%
      select(SAMPLE_POINT_ID, LONGITUDE, LATITUDE)

    # Lookup tables  
    t_taxonomy_codes <<- tbl(connection, in_schema("NIVADATABASE", "TAXONOMY_CODES")) %>%
      select(TAXONOMY_CODE_ID, CODE, NIVA_TAXON_ID)
    t_taxonomy <<- tbl(connection, in_schema("NIVADATABASE", "TAXONOMY")) %>%
      select(NIVA_TAXON_ID, LATIN_NAME)
    t_tissue <<- tbl(connection, in_schema("NIVADATABASE", "BIOTA_TISSUE_TYPES")) %>%
      select(TISSUE_ID, TISSUE_NAME)
    t_lims_id <<- tbl(connection, in_schema("NIVADATABASE", "LABWARE_BSID")) %>%
      select(BIOTA_SAMPLE_ID, LABWARE_TEXT_ID)
  }
  
}


if (FALSE){
  # Test
  # debugonce(define_nivabasen_tables)
  define_nivabasen_tables(con)
  
  test <- tbl(con, in_schema("NIVADATABASE", "STATIONS")) %>%
    select(STATION_ID, GEOM_REF_ID)
  
  t_projects %>% head(3)
  t_projects %>%
    filter(PROJECT_NAME == 'CEMP_Biota') %>%
    collect()
  # Same, using SQL syntax:
  t_projects %>%
    filter(sql("PROJECT_NAME = 'CEMP_Biota'")) %>%
    collect()
  t_projects %>%
    filter(substr(PROJECT_NAME,1,4) %in% "CEMP") %>%
    collect()
  # NOTE: grepl, example below, doesnt work with dbplyr:
  #   filter(grepl("CEMP", PROJECT_NAME))
  # Must use SQL syntax, put inside sql(), instead:
  t_projects %>%
    filter(sql("PROJECT_NAME like '%CEMP%'")) %>%
    collect()
}




define_biotachemistry_tables <- function(connection, project_id = 3699, ask_before_overwrite = T){
  
  require(dbplyr)
  
  object_names <- c("t_specimenlevel", "t_samplelevel")
  t_projects_with_onumbers <- t_projects %>%
    filter(PROJECT_ID %in% project_id) %>%
    left_join(t_projects_onumbers, by = join_by(PROJECT_ID))   
  
  cat("\nCreating tables", paste(sQuote(object_names), collapse = ","), "based on the following projects:\n\n")
  print(t_projects_with_onumbers)
  cat("\n")
  
  cat("\n\nNOTE: the data will not be downloaded when you do this (lazy loading).\n\n")
  
  # Find existing variables in the global environment ("n = 1") starting with "t_"
  existing_object_names <- ls(name = 1, pattern = "^t_")
  overlap <- intersect(object_names, existing_object_names)
  
  create_objects <- TRUE
  if (length(overlap) > 0 & ask_before_overwrite){ 
    answer <- ""
    while (!answer %in% c("y","n")){
      cat("This will overwrite the following existing objects:\n")
      print(overlap)
      answer <- readline("OK to continue? (y/n)")
      if (answer == "n"){
        create_objects <- FALSE
        break()
      }
    }
  }
  
  if (create_objects){
    
    # Specimen level
    # - one row per specimen, in cas of poole dsamples there wil be several rows per sample
    t_specimenlevel <<- t_specimens %>%
      mutate(
        YEAR = year(DATE_CAUGHT),
        MONTH = month(DATE_CAUGHT),
        MYEAR = case_when(
          MONTH >= 4 ~ YEAR,
          MONTH < 4 ~ YEAR-1)) %>%
      left_join(t_samples_specimens, by = join_by(SPECIMEN_ID)) %>%
      left_join(t_samples, by = join_by(SAMPLE_ID)) %>% 
      left_join(t_tissue, by = join_by(TISSUE_ID)) %>% 
      left_join(t_lims_id, by = c("SAMPLE_ID" = "BIOTA_SAMPLE_ID")) %>% 
      left_join(t_measurements, by = join_by(SAMPLE_ID)) %>%
      left_join(t_methods, by = join_by(METHOD_ID)) %>%
      left_join(t_basis, by = join_by(BASIS_ID)) %>%
      left_join(t_project_stations, by = join_by(STATION_ID)) %>%
      left_join(t_projects, by = join_by(PROJECT_ID)) %>%
      left_join(t_stations, by = join_by(STATION_ID)) %>%
      left_join(t_coordinates, by = join_by(GEOM_REF_ID == SAMPLE_POINT_ID)) %>%
      filter(PROJECT_ID %in% project_id)  %>%
      left_join(t_taxonomy_codes, join_by(TAXONOMY_CODE_ID)) %>%
      left_join(t_taxonomy)  
    
    # Sample level
    # - one row per sample, specimen-level info is discarded  
    # - assumes that there is only one project, station and MYEAR per sample  
    t_samplelevel <<- t_specimenlevel %>%
      distinct(
        # Main data
        PROJECT_NAME, STATION_CODE, STATION_NAME, LATIN_NAME, MYEAR, TISSUE_NAME, SAMPLE_NO, REPNO, 
        NAME, VALUE, FLAG1, UNIT, BASIS_CODE,
        # extra time info
        # DATE_CAUGHT, YEAR, MONTH,
        # extra sample info (note: LABWARE_TEXT_ID not included as there can be two or more TEXT_ID for a single sample)
        REMARK_sample, 
        # extra chemical methiod info
        LABORATORY, METHOD_REF, DETECTION_LIMIT, UNCERTAINTY, QUANTIFICATION_LIMIT, 
        # IDs
        PROJECT_ID, STATION_ID, TAXONOMY_CODE_ID, MATRIX_ID, SAMPLE_ID, TISSUE_ID, METHOD_ID, VALUE_ID)

  }
  
}



if (FALSE){
  # Test
  # debugonce(define_biotachemistry_tables)
  define_biotachemistry_tables(con)
  define_biotachemistry_tables(connection = con, project_id = 12820, ask_before_overwrite = F)
}


get_biotachemistry <- function(connection, download_data = FALSE, year = NULL, filter_sql = NULL){
  
  object_names <- c("t_specimenlevel", "t_samplelevel")
  existing_object_names <- ls(name = 1, pattern = "^t_")
  objects_lacking <- setdiff(object_names, existing_object_names)
  if (length(objects_lacking) > 0){
    message("The following objects must be defined. Run 'define_biotachemistry_tables()' to define them, then you can try this function again.\n")
    print(object_names)
    stop("Lacking objects ", paste(sQuote(objects_lacking), collapse = ","))
  }

  if (is.null(year)){
    t_samplelevel_filtered <- t_samplelevel
  } else {
    t_samplelevel_filtered <- t_samplelevel %>%
      filter(MYEAR %in% year)
  }
  
  if (!is.null(filter_sql)){
    t_samplelevel_filtered <- t_samplelevel_filtered %>%
      filter(sql(filter_sql))
  }
  
  rows_by_year <- t_samplelevel_filtered %>%
    count(MYEAR) %>%
    collect()
  number_of_samples <- sum(rows_by_year$n)

  if (download_data){
    
    t0 <- now()
    
    t_samplelevel_filtered <- t_samplelevel_filtered %>%
      collect()
    
    sampleids <- unique(t_samplelevel_filtered$SAMPLE_ID)
    
    t_specimens_pooled <- t_specimenlevel %>%
      filter(SAMPLE_ID %in% sampleids) %>%
      distinct(STATION_CODE, MYEAR, YEAR, MONTH, TISSUE_NAME, SAMPLE_ID, SPECIMEN_ID, SPECIMEN_NO, STATION_ID, 
               LABWARE_TEXT_ID, PROJECT_ID, PROJECT_NAME, LONGITUDE, LATITUDE) %>%
      collect() %>%
      arrange(STATION_CODE, MYEAR, TISSUE_NAME, SAMPLE_ID, SPECIMEN_NO, LABWARE_TEXT_ID) %>%
      group_by(STATION_CODE, MYEAR, TISSUE_NAME, SAMPLE_ID,
               STATION_ID, LONGITUDE, LATITUDE) %>%
      summarise(
        Pooled_n = n(),
        LABWARE_TEXT_ID = stringr::str_flatten(unique(LABWARE_TEXT_ID), collapse = ", "),
        SPECIMEN_ID = stringr::str_flatten(unique(SPECIMEN_ID), collapse = ", "),
        SPECIMEN_NO = stringr::str_flatten(unique(SPECIMEN_NO), collapse = ", "),
        Year = mean(YEAR),
        Month = mean(MONTH),
        .groups = "drop"
      ) 
    
    # how long time took the download?  
    t1 <- now()
    cat("Time used for download:  "); print(t1-t0)
    
    result <- t_samplelevel_filtered %>%
      left_join(
        t_specimens_pooled %>% select(SAMPLE_ID, Pooled_n, LABWARE_TEXT_ID, SPECIMEN_ID, SPECIMEN_NO, Year, Month,
                                      STATION_ID, LONGITUDE, LATITUDE),
        by = "SAMPLE_ID")
    
  } else {
    
    result <- rows_by_year
    
  }
  
  message("number of samples: ", number_of_samples)
  invisible(result)
  
}

if (FALSE){
  # Test
  # debugonce(get_biotachemistry)
  x <- get_biotachemistry(con, year = 2020:2023)
  x <- get_biotachemistry(con, year = 2020:2023, filter_sql = "NAME = 'Mirex'")
  x <- get_biotachemistry(con, year = 2022:2023, filter_sql = "NAME = 'Mirex'", download_data = TRUE)
  
  # Ranfjorden: O-240130
  # Høyangsfjorden: O-240237
  # Kristiansandsfjorden: O-240244.
  proj <- find_projects_onumber(240130)  # Ransfjord
  define_biotachemistry_tables(connection = con, project_id = proj$PROJECT_ID, ask_before_overwrite = F)
  get_biotachemistry(con, year = 2024)
  # debugonce(get_biotachemistry)
  dat_ransfjord <- get_biotachemistry(con, year = 2024, download_data = TRUE)
  xtabs(~STATION_CODE + MYEAR, dat_ransfjord)
  xtabs(~Month + MYEAR, dat_ransfjord)
  #  saveRDS(dat_ransfjord, "App02_Industry_data/data_chem_industry_ransfjord_2024.rds")
  
  proj <- find_projects_onumber(240237)  # hoyangsfjord
  define_biotachemistry_tables(connection = con, project_id = proj$PROJECT_ID, ask_before_overwrite = F)
  get_biotachemistry(con, year = 2024)
  # debugonce(get_biotachemistry)
  dat_hoyangsfjord <- get_biotachemistry(con, year = 2024, download_data = TRUE)
  xtabs(~STATION_CODE + MYEAR, dat_hoyangsfjord)
  library(leaflet)
  dat_hoyangsfjord %>%
    arrange(STATION_CODE, LONGITUDE, LATITUDE, MYEAR) %>%
    group_by(STATION_CODE, LONGITUDE, LATITUDE) %>%
    summarise(Years = paste(unique(MYEAR), collapse = ","))
  leaflet(dat_hoyangsfjord) %>% 
    addTiles() %>% 
    addMarkers(lng = LONGITUDE, lat = LATITUDE, popup = paste(STATION_CODE, MYEAR))
  # saveRDS(dat_hoyangsfjord, "App02_Industry_data/data_chem_industry_hoyangsfjord_2024.rds")
  
  # Several years
  # find_projects("høyang", wildcard = TRUE, ignore.case = TRUE)  # hoyangsfjord
  proj_id <- c(12375, 12633, 12820) # 2018, 2021, 2024
  define_biotachemistry_tables(connection = con, project_id = proj_id, ask_before_overwrite = F)
  get_biotachemistry(con, year = 2018:2024)
  # debugonce(get_biotachemistry)
  dat_hoyangsfjord <- get_biotachemistry(con, year = 2018:2024, download_data = TRUE)
  

  dat_hoyangsfjord %>% count(NAME) %>% View()
  dat_hoyangsfjord %>% filter(NAME == "Kvikksølv") %>%
    count(STATION_CODE, MYEAR)
  
    check <- tbl(con, in_schema("NIVADATABASE", "LABWARE_CHECK_SAMPLE")) %>% head(3)
    count(ACCOUNT_NUMBER, PROSJEKT, CUSTOMER) %>%
    filter(CUSTOMER == 'SIX') %>%
    collect()
  
  proj <- find_projects_onumber(240244)  # Kristiansandsfjorden
  define_biotachemistry_tables(connection = con, project_id = proj$PROJECT_ID, ask_before_overwrite = F)
  get_biotachemistry(con, year = 2024)
  # debugonce(get_biotachemistry)
  dat_kristiansand <- get_biotachemistry(con, year = 2024, download_data = TRUE)
  
}

#
# select project by O_NUMBER, PROJECT_ID, STATION_CODE or STATION_NAME
# - only PROJECT_ID and O_NUMBER implemented so far
# - can use several values for project_id, and for o_number if exact = TRUE
#
select_projects_stations <- function(o_number = NULL, 
                                     project_id = NULL, 
                                     station_code = NULL,
                                     station_name = NULL,
                                     exact = FALSE, 
                                     connection){
  result <- tbl(connection, in_schema("NIVADATABASE", "PROJECTS_STATIONS")) %>%
    select(PROJECT_ID, STATION_ID, STATION_CODE, STATION_NAME, PROJECTS_STATION_ID) %>% 
    # add O_NUMBER column to data
    left_join(
      tbl(con, in_schema("NIVADATABASE", "PROJECTS_O_NUMBERS")) %>% 
        select(PROJECT_ID, O_NUMBER),
      by = join_by(PROJECT_ID)
    )
  # project_id has preference over O-number (and is always expected to be exact)
  if (!is.null(project_id)){
    result <- result %>%
      filter(PROJECT_ID %in% project_id)
  # if project_id is not given, search for O-number will be used
  # you must only gove 
  } else if (!is.null(o_number)){
    if (length(o_number) > 1 & !exact){
      warning("More than one O-number given - exact search was used")
      exact <- TRUE
    }
    if (exact){
      result <- result %>%
        filter(O_NUMBER %in% o_number)
    } else {
      sql_code <- paste0("O_NUMBER LIKE ", sQuote(paste0("%", o_number, "%")))
      result <- result %>%
        filter(sql(sql_code))
    }
  }
  
  # In addition, you may search for station code   
  if (!is.null(station_code)){
    if (exact){
      result <- result %>% filter(STATION_CODE %in% station_code)
    } else {
      sql_code <- paste0("STATION_CODE LIKE ", sQuote(paste0("%", station_code, "%")))
      result <- result %>% filter(sql(sql_code))
    }
  }
  
  # In addition, you may search for station name   
  if (!is.null(station_name)){
    if (exact){
      result <- result %>% filter(STATION_NAME %in% station_name)
    } else {
      sql_code <- paste0("STATION_NAME LIKE ", sQuote(paste0("%", station_name, "%")))
      result <- result %>% filter(sql(sql_code))
    }
  }  
    
  result
  
}

if (FALSE){
  # Search for O-number with non-exact search 
  select_projects_stations(o_number = "240237", connection = con)
  select_projects_stations(o_number = "O-21", connection = con)
  select_projects_stations(station_name = "Mississippibekken", connection = con)
  select_projects_stations(station_code = "Nordlys", connection = con) %>% 
    left_join(tbl(con, in_schema("NIVADATABASE", "PROJECTS")) %>% 
                filter(PROJECT_ID %in% df1$PROJECT_ID) %>% 
                select(PROJECT_ID, PROJECT_NAME),
              by = join_by(STATION_ID)
    )
  # Search for several O-numbers
  find_projects("høyang", wildcard = TRUE, ignore.case = TRUE, connection = con)
  # - using exact search (the first one gives a warning)
  select_projects_stations(o_number = c("180293", "210293", "240237"), connection = con)
  select_projects_stations(o_number = c("180293", "210293", "240237"), exact = TRUE, connection = con)
}


#
# select project by STATION_ID, coordinates or prorject info
# only STATION_ID implemented so far
#
select_stations <- function(station_id = NULL, ..., connection){
  # Define stations by starting with projects  
  result <- select_projects_stations(..., connection = connection) %>%
    # summarize project information  
    group_by(STATION_ID) %>% 
    summarize(
      STATION_CODEs = sql("listagg(unique(STATION_CODE), ', ') within group (order by PROJECT_ID)"),
      STATION_NAMEs = sql("listagg(unique(STATION_NAME), ', ') within group (order by PROJECT_ID)"),
      O_NUMBERs = sql("listagg(unique(O_NUMBER), ',') within group (order by PROJECT_ID)"),
      PROJECT_IDs = sql("listagg(PROJECT_ID, ',') within group (order by PROJECT_ID)")) %>%
    # add coordinate columns
    left_join(
      tbl(connection, in_schema("NIVADATABASE", "STATIONS")) %>% select(STATION_ID, GEOM_REF_ID),
      by = join_by(STATION_ID)) %>% 
    left_join(
      tbl(connection, in_schema("NIVA_GEOMETRY", "SAMPLE_POINTS")) %>% select(SAMPLE_POINT_ID, LONGITUDE, LATITUDE),
      by = join_by(GEOM_REF_ID == SAMPLE_POINT_ID))
  if (!is.null(station_id)){
    result <- result %>%
      filter(STATION_ID %in% station_id)
  }
  result
}
 

if (FALSE){
  # test
  # - returns one station, and all projects with this station
  select_stations(station_id = 50588, connection = con)
  # - returns several stations, and all projects with these stations
  select_stations(station_id = 50588:50589, connection = con)
  select_stations(station_id = c(), connection = con)
  # - returns 5 stations, and only the given project 
  select_stations(o_number = "240237", connection = con)
}


#
# select specimen by specimen_id, date, year or species, or project/station info
# only specimen_id, myear and species implemented so far
#
select_specimens <- function(specimen_id = NULL, myear = NULL, species = NULL, station_id = NULL, o_number = NULL, connection){
  # Define stations by srtarting with projects  
  result <- select_stations(station_id = station_id, o_number = o_number, connection = connection) %>%
    left_join(
      tbl(connection, in_schema("NIVADATABASE", "BIOTA_SINGLE_SPECIMENS")) %>%
        select(STATION_ID, SPECIMEN_ID, SPECIMEN_NO, DATE_CAUGHT, TAXONOMY_CODE_ID, REMARK) %>%
        rename(REMARK_specimen = REMARK),
      by = join_by(STATION_ID)) %>%
    mutate(
      YEAR = year(DATE_CAUGHT),
      MONTH = month(DATE_CAUGHT),
      MYEAR = case_when(
        MONTH >= 4 ~ YEAR,
        MONTH < 4 ~ YEAR-1)) %>%
    left_join(
      tbl(connection, in_schema("NIVADATABASE", "TAXONOMY_CODES")) %>%
        select(TAXONOMY_CODE_ID, CODE, NIVA_TAXON_ID),
      by = join_by(TAXONOMY_CODE_ID)) %>%
    left_join(
      tbl(connection, in_schema("NIVADATABASE", "TAXONOMY")) %>%
        select(NIVA_TAXON_ID, LATIN_NAME),
      by = join_by(NIVA_TAXON_ID))

  if (!is.null(specimen_id)){
    result <- result %>%
      filter(SPECIMEN_ID %in% specimen_id)
  }
  
  if (!is.null(myear)){
    result <- result %>%
      filter(MYEAR %in% myear)
  }
  
  if (!is.null(species)){
    result <- result %>%
      filter(LATIN_NAME %in% species)
  }
  
  result
}


if (FALSE){
  # test
  # - returns 10 specimens, 1-3 per year
  select_specimens(station_id = 50588, connection = con) %>%
    xtabs(~DATE_CAUGHT + STATION_CODEs, .)
  # - returns all specimens for the stations used by the o_number,
  # both specimens taken in the given project and earlier specimens
  select_specimens(o_number = "240237", connection = con) %>%
    xtabs(~DATE_CAUGHT + STATION_CODEs + PROJECT_IDs, .)
  # in order to get only specimens from the given project, also add year
  select_specimens(o_number = "240237", myear = 2024, connection = con) %>%
    xtabs(~DATE_CAUGHT + STATION_CODEs + PROJECT_IDs, .)
  # - returns just the given specimen, but project info for earlier projects
  # as well
  select_specimens(specimen_id = 325106, connection = con) %>%
    xtabs(~DATE_CAUGHT + STATION_CODEs + PROJECT_IDs, .)
  # - returns all specimens for the given species
  select_specimens(species = "Mytilus edulis", connection = con) %>%
    count(LATIN_NAME)
  # - returns all specimens for the given species in the given year
  select_specimens(species = "Mytilus edulis", myear = 2024, connection = con) %>%
    count(LATIN_NAME)
  # - returns all specimens for the given species in the given year
  test <- select_specimens(station_ = "15B", myear = 2023, connection = con)
  colnames(test)
  count(test, LATIN_NAME)
}

# t_project_stations %>% filter(STATION_CODE == "15B")

#
# select specimen by specimen_id, date, year or species, or project/station info
# only specimen_id, myear and species implemented so far
#
# drop STATION_ID, TAXONOMY_CODE_ID from samples
# Lookup tables  
# t_taxonomy_codes <<- tbl(connection, in_schema("NIVADATABASE", "TAXONOMY_CODES")) %>%
#   select(TAXONOMY_CODE_ID, CODE, NIVA_TAXON_ID)
# t_taxonomy <<- tbl(connection, in_schema("NIVADATABASE", "TAXONOMY")) %>%
#   select(NIVA_TAXON_ID, LATIN_NAME)
# t_tissue <<- tbl(connection, in_schema("NIVADATABASE", "BIOTA_TISSUE_TYPES")) %>%
#   select(TISSUE_ID, TISSUE_NAME)
# t_lims_id <<- tbl(connection, in_schema("NIVADATABASE", "LABWARE_BSID")) %>%
#   select(BIOTA_SAMPLE_ID, LABWARE_TEXT_ID)


select_samples <- function(sample_id = NULL,
                           tissue = NULL,
                           specimen_id = NULL, 
                           myear = NULL, 
                           species = NULL, 
                           station_id = NULL, 
                           o_number = NULL, 
                           stop_if_problem = TRUE, 
                           connection){
  # Define stations by srtarting with projects  
  result <- select_specimens(specimen_id = specimen_id, 
                            myear = myear, 
                            species = species, 
                            station_id = station_id, 
                            o_number = o_number, 
                            connection = connection) %>%
    # select(SPECIMEN_ID, SPECIMEN_NO, MYEAR) %>% 
    left_join(
      tbl(connection, in_schema("NIVADATABASE", "BIOTA_SAMPLES_SPECIMENS")) %>%
        select(SPECIMEN_ID, SAMPLE_ID),
      by = join_by(SPECIMEN_ID)) %>%
    left_join(
      tbl(connection, in_schema("NIVADATABASE", "BIOTA_SAMPLES")) %>%
        select(SAMPLE_ID, TISSUE_ID, SAMPLE_NO, REPNO, REMARK) %>%
        rename(REMARK_sample = REMARK),
      by = join_by(SAMPLE_ID)) %>%
  # need the next line, as stations with no biota data (e.g. sediment stations)
  # will produce a line with SAMPLE_ID = NA 
  filter(!is.na(SAMPLE_ID)) %>%
  group_by(SAMPLE_ID, STATION_CODEs, TISSUE_ID, SAMPLE_NO, REPNO, REMARK_sample) %>% 
    summarize(
      STATION_ID_mean = mean(STATION_ID, na.rm = TRUE),
      STATION_ID_max = max(STATION_ID, na.rm = TRUE),
      STATION_ID = sql("listagg(unique(STATION_ID), ',') within group (order by SPECIMEN_NO)"),
      SPECIMEN_IDs = sql("listagg(unique(SPECIMEN_ID), ',') within group (order by SPECIMEN_NO)"),
      SPECIMEN_NOs = sql("listagg(unique(SPECIMEN_NO), ',') within group (order by SPECIMEN_NO)"),
      SAMPLE_DATE = median(DATE_CAUGHT, na.rm = TRUE),
      SAMPLE_DATE_min = min(DATE_CAUGHT, na.rm = TRUE),
      SAMPLE_DATE_max = max(DATE_CAUGHT, na.rm = TRUE), 
      # LATIN_NAME_n = length(unique(LATIN_NAME)),
      LATIN_NAMEs = sql("listagg(unique(LATIN_NAME), ',') within group (order by SPECIMEN_NO)"),
      .groups = "drop") %>%
    mutate(
      YEAR = year(SAMPLE_DATE),
      MONTH = month(SAMPLE_DATE),
      MYEAR = case_when(
        MONTH >= 4 ~ YEAR,
        MONTH < 4 ~ YEAR-1)
    ) %>%
    left_join(
      tbl(connection, in_schema("NIVADATABASE", "BIOTA_TISSUE_TYPES")) %>%
        select(TISSUE_ID, TISSUE_NAME),
      by = join_by(TISSUE_ID))
  
    # for later: add more specimen/ project info?
  
  # select rows by SAMPLE_ID
  if (!is.null(sample_id)){
    result <- result %>%
      filter(SAMPLE_ID %in% sample_id)
  }
  
  # select rows by TISSUE_NAME
  if (!is.null(tissue)){
    result <- result %>%
      filter(TISSUE_NAME %in% tissue)
  }
  
  # Check samples that mixes different sample dats for the specimens  
  check <- result %>%
    filter(SAMPLE_DATE_max > SAMPLE_DATE_min) %>%
    collect()
  if (nrow(check) > 0){
    warning(nrow(check), " samples are polled samples where the sample date varies among specimens")
    message("Maximum time difference between specimens in one sample is:")
    diff <- max(check$SAMPLE_DATE_max - check$SAMPLE_DATE_min)
    print(diff)
  }

  # Check whether there is only one station per sample
  # Default behaviour (stop_if_problem = TRUE): the function fails with error message
  # Optional behaviour (stop_if_problem = FALSE): just give a warning message
  check <- result %>%
    filter(STATION_ID_max > STATION_ID_mean) %>%
    collect()
  if (nrow(check) > 0 & stop_if_problem){
    stop(nrow(check), " samples are from more than one station (STATION_ID)!")
  } else if (nrow(check) > 0 & !stop_if_problem){
    warning(nrow(check), " samples are from more than one station (STATION_ID)!")
    result <- result %>%
      mutate(PROBLEM_station = case_when(
        STATION_ID_max > STATION_ID_mean ~ "Sample is from more than one station!"))
  } else {
    result <- result %>%
      select(-STATION_ID_mean, -STATION_ID_max) %>%
      mutate(STATION_ID = as.numeric(STATION_ID))
  }
  
  # Check whether there is only one species per sample
  # Default behaviour (stop_if_problem = TRUE): the function fails with error message
  # Optional behaviour (stop_if_problem = FALSE): just give a warning message
  check <- result %>%
    count(LATIN_NAMEs) %>%
    collect() %>%
    filter(grepl(",", LATIN_NAMEs, fixed = TRUE))
  if (nrow(check) > 0 & stop_if_problem){
    stop(nrow(check), " samples are from more than one species (LATIN_NAME)!")
  } else if (nrow(check) > 0 & !stop_if_problem){
    warning(nrow(check), " samples are from more than one species (LATIN_NAME)!")
    result <- result%>%
      rename(LATIN_NAME = LATIN_NAMEs) %>%
      mutate(PROBLEM_species = case_when(
        LATIN_NAME %in% check$LATIN_NAMEs ~ "Sample is from more than one species!"))
  } else {
    result <- result %>%
      rename(LATIN_NAME = LATIN_NAMEs)
  }
  
  # select rows by LATIN_NAME
  if (!is.null(species)){
    result <- result %>%
      filter(LATIN_NAME %in% species)
  }
  
  check <- result %>%
    count(SAMPLE_ID) %>%
    filter(n > 1) %>%
    collect()
  
  if (nrow(check) > 0){
    stop(nrow(check), " samples (SAMPLE_ID) occur more than once!")
  }

  result
}

if (FALSE){
  # debugonce(select_samples)
  test <- select_samples(specimen_id = 325106, connection = con)
  # in this case, just one sample
  xtabs(~DATE_CAUGHT + STATION_CODEs + PROJECT_IDs, test)
  # search using project and year
  test <- select_samples(o_number = "240237", myear = 2024, connection = con)
    # test %>% collect() %>% View()
  xtabs(~DATE_CAUGHT + STATION_CODEs + PROJECT_IDs, test)
  # search using project only - returns all years (but no sample duplicates)
  test <- select_samples(o_number = "240237", connection = con)
  xtabs(~DATE_CAUGHT + STATION_CODEs + PROJECT_IDs, test)
  # Milkys - contains pooled samples (multiple specimens per sample)
  find_projects("CEMP", wildcard = TRUE, connection = con)  
  test <- select_samples(o_number = "14330ANA", myear = 2016:2023, connection = con)
  test %>% count(LATIN_NAME)
  # search using species
  test <- select_samples(o_number = "14330ANA", myear = 2023, species = "Mytilus edulis", 
                         connection = con)
  test %>% count(STATION_CODEs)
  # search using sample_id
  test <- select_samples(sample_id = 239537, connection = con)
  collect(test)
  test <- select_samples(sample_id = 239537, connection = con)
  collect(test)
  # test a startion with pooled samples  
  df1 <- select_stations(o_number = "14330ANA", connection = con) %>% collect()
  df2 <- select_specimens(station_id = 46980, myear = 2023, connection = con) %>% collect() 
  df3 <- select_samples(station_id = 46980, myear = 2023, tissue = "Lever", connection = con) %>% collect()
}



#
# NOTE: would be a good idea to have a separate function for *downloading*,
# as an alternative to select_measurements() %>% collect()
# As it is now, downloading is slower than needed as we download all
# the sample, specimen and station metadata repetated X times (for each
# measurement), instead of downloading them separately and then joining them.
# OR rewrite the function with "download = FALSE/TRUE".  
#
select_measurements <- function(value_id = NULL,
                                param = NULL,
                                sample_id = NULL,
                           tissue = NULL,
                           specimen_id = NULL, 
                           myear = NULL, 
                           species = NULL, 
                           station_id = NULL, 
                           o_number = NULL, 
                           stop_if_problem = TRUE,
                           connection){
  
  # drop STATION_ID, TAXONOMY_CODE_ID from samples
  result <- select_samples(sample_id = NULL,
                           specimen_id = specimen_id, 
                             myear = myear, 
                             species = species, 
                             station_id = station_id, 
                             o_number = o_number, 
                           stop_if_problem = stop_if_problem,
                             connection = connection) %>%
    # select(SPECIMEN_ID, SPECIMEN_NO, MYEAR) %>% 
    left_join(
      tbl(connection, in_schema("NIVADATABASE", "BIOTA_CHEMISTRY_VALUES")) %>%
        select(SAMPLE_ID, METHOD_ID, VALUE_ID, VALUE, FLAG1, 
               DETECTION_LIMIT, UNCERTAINTY, QUANTIFICATION_LIMIT),
      by = join_by(SAMPLE_ID)) %>%
    left_join(
      tbl(connection, in_schema("NIVADATABASE", "METHOD_DEFINITIONS")) %>%
        select(METHOD_ID, NAME, UNIT, LABORATORY, METHOD_REF, MATRIX_ID, BASIS_ID),
      by = join_by(METHOD_ID)) %>% 
    left_join(
      tbl(connection, in_schema("NIVADATABASE", "MATRIX_DEFINITIONS")) %>%
        select(MATRIX_ID, MATRIX_NAME),
      by = join_by(MATRIX_ID)) %>% 
    left_join(
      tbl(connection, in_schema("NIVADATABASE", "BASIS_DEFINITIONS")) %>%
        select(BASIS_ID, BASIS_CODE),
      by = join_by(BASIS_ID)) 
  
  # for later: add more specimen/ project info?
  
  if (!is.null(value_id)){
    result <- result %>%
      filter(VALUE_ID %in% value_id)
  }
  if (!is.null(param)){
    result <- result %>%
      filter(NAME %in% param)
  }
  
  result
}

if (FALSE){
  # debugonce(select_measurements)
  test <- select_measurements(specimen_id = 325106, connection = con)
  # in this case, just one sample
  xtabs(~MYEAR + STATION_ID, test)
  # search using project, year and parameters  
  test <- select_measurements(o_number = "240237", myear = 2024, 
                         param = c("Kvikksølv", "Bly"), connection = con)
  
  # test %>% collect() %>% View()
  xtabs(~STATION_ID + NAME, test)
  # Milkys - select all mercury measurements in a year  
  find_projects("CEMP", wildcard = TRUE, connection = con)  
  test <- select_measurements(o_number = "14330ANA", myear = 2023, 
                         param = "Kvikksølv", connection = con)
  test %>% count(STATION_ID)  
  library(ggplot2)
  test %>% 
    group_by(STATION_ID) %>%
    summarize(median_hg = median(VALUE)) %>% 
    ggplot(aes(factor(STATION_ID), median_hg)) + 
    geom_col()
  
}



find_projects <- function(search_text = NULL, id = NULL, wildcard = FALSE, ignore.case = FALSE, connection){
  
  options(useFancyQuotes = FALSE)
  
  if (!wildcard & !ignore.case){
    sql_syntax <- paste0("PROJECT_NAME = ", sQuote(search_text))
  } else if (!wildcard & ignore.case){
    sql_syntax <- paste0("LOWER(PROJECT_NAME) = ", sQuote(tolower(search_text)))
  } else if (wildcard & !ignore.case){
    sql_syntax <- paste0("PROJECT_NAME LIKE ", sQuote(paste0("%", search_text, "%")))
  } else if (wildcard & ignore.case){
    sql_syntax <- paste0("LOWER(PROJECT_NAME) LIKE ", sQuote(paste0("%", tolower(search_text), "%")))
  }

  result <- tbl(connection, in_schema("NIVADATABASE", "PROJECTS")) %>%
    select(PROJECT_ID, PROJECT_NAME, PROJECT_DESCRIPTION) %>% 
    filter(sql(sql_syntax)) %>% 
    left_join(tbl(connection, in_schema("NIVADATABASE", "PROJECTS_O_NUMBERS")) %>%
                select(PROJECT_ID, O_NUMBER), 
              by = join_by(PROJECT_ID)) %>%
    collect()
  
  result
}

if (FALSE){
  # debugonce(find_projects)
  find_projects("CEMP_Biota", connection = con)
  find_projects("CEMP_biota", connection = con)
  find_projects("CEMP_biota", ignore.case = TRUE, connection = con)
  find_projects("CEMP", wildcard = TRUE, connection = con)
  find_projects("cemp", wildcard = TRUE, connection = con)
  find_projects("cemp", wildcard = TRUE, ignore.case = TRUE, connection = con)
  find_projects("høyang", wildcard = TRUE, ignore.case = TRUE, connection = con)
}


find_projects_onumber <- function(search_text = NULL, id = NULL, wildcard = FALSE, ignore.case = FALSE){
    
    options(useFancyQuotes = FALSE)
    
    if (!wildcard & !ignore.case){
      sql_syntax <- paste0("O_NUMBER = ", sQuote(search_text))
    } else if (!wildcard & ignore.case){
      sql_syntax <- paste0("LOWER(O_NUMBER) = ", sQuote(tolower(search_text)))
    } else if (wildcard & !ignore.case){
      sql_syntax <- paste0("O_NUMBER LIKE ", sQuote(paste0("%", search_text, "%")))
    } else if (wildcard & ignore.case){
      sql_syntax <- paste0("LOWER(O_NUMBER) LIKE ", sQuote(paste0("%", tolower(search_text), "%")))
    }
    
    result <- t_projects %>% 
      left_join(t_projects_onumbers, by = join_by(PROJECT_ID)) %>%
      filter(sql(sql_syntax)) %>% 
      collect()
    
    result
  }
  

if (FALSE){
  # Test
  find_projects_onumber("14330ANA")
  find_projects_onumber(14330)
  find_projects_onumber(14330, wildcard = TRUE)
  # Ranfjorden: O-240130
  # Høyangsfjorden: O-240237
  # Kristiansandsfjorden: O-240244.
  find_projects_onumber(240237, wildcard = TRUE)
}








