

list_labware_files <- function(year){
  pattern <- paste0("Labware_samples_", year)
  files <- dir("Input_data", pattern) %>% rev()
  cat("Year", year, "has", length(files), "files to choose from \n")
  files
}



# Function for getting the number of individuals (per pooled sample) from X_BULK_BIO 
# the function is based on counting the number of commas
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