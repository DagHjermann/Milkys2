
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




