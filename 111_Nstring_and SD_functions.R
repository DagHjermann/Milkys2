

list_files <- function(folder, pattern){
  files <- dir(folder, pattern) %>% rev()
  cat("There are", length(files), "files with pattern", sQuote(pattern), "to choose from \n")
  files
}

list_labware_files <- function(year){
  pattern <- paste0("Labware_samples_", year)
  files <- dir("Input_data", pattern) %>% rev()
  cat("Year", year, "has", length(files), "files to choose from \n")
  files
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
    "109_adjusted_data_2020-07-03", "109_adjusted_data_2020-06-12", 
    "109_adjusted_data_2020-05-29")
  X <- read_rds_file("Data",
                     files, filenumber = 5,   # gets the file from May 2020
                     get_date = TRUE, time_since_modified = TRUE)
  the_data_itself <- X$data  # get data
  X$file_date                # "2020-05-29"
  
}


# Function for getting the number of individuals (per pooeld sample) from X_BULK_BIO 
# the function is based on counting the number of commas
number_of_ind <- function(txt){
  searchresult <- gregexpr(",", txt, fixed = TRUE)
  result <- searchresult %>% 
    map_int(length)
  result_na <- searchresult %>% 
    map(is.na) %>% 
    map_lgl(~.[1])
  result_notfound <- searchresult %>% 
    map(~(. == -1)) %>% 
    map_lgl(~.[1])
  list(result, result_na, result_notfound)
  # is.na(searchresult) %>% print()
  # print(searchresult == (-1))
  result
  result <- case_when(
    is.na(searchresult) ~ 1,
    searchresult == (-1) ~ 1,
    TRUE ~ length(searchresult) + 1
  )
  result[1]
}
number_of_ind <- function(txt){
  searchresult <- gregexpr(",", txt, fixed = TRUE)
  result <- searchresult %>% 
    map_int(length) + 1
  result_na <- searchresult %>% 
    map(is.na) %>% 
    map_lgl(~.[1])
  result_no_comma <- searchresult %>% 
    map(~(. == -1)) %>% 
    map_lgl(~.[1])
  # list(result, result_na, result_no_comma)
  result[result_no_comma] <- 1
  result[result_na] <- NA
  result
}

if (FALSE){
  # Test
  number_of_ind(c("8,5,7,10", "8,5,4", "8,5", "5", as.character(NA))) 
}