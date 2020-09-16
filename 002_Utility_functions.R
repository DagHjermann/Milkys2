
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
    pos1 <- regexpr("20[0-9][0-9]", filename)   # search for the first place "20" plus two numbers occurs inside filename
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
