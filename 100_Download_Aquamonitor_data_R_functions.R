
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# DOWNLOADING AQUAMONITOR DATA ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

set_password <- function () 
  
{
  db_username <- svDialogs::dlg_input("Please write your username (capital letters)", "DHJ")$res 
  db_pwd <- getPass::getPass("Please write your password")
  
  invisible(list(usr = db_username, pwd = db_pwd))
  
}

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# READING "WIDE" AQUAMONITOR DATA ----
#
# - Puts the data on 'long' format (two variables named Substance and Value)
# - Additional variables for Unit and Flag ("<" for values less than LOQ)
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

AqMexport_read_chemistry_table <- function(filename, 
                                     sheetname = "BiotaChemistry",
                                     folder = "Data_Nivabasen",
                                     reformat_long = TRUE, 
                                     remove_duplicates = TRUE){
  fn <- paste0(folder, "/", filename)
  if (sheetname == "WaterChemistry"){ 
    number_of_key_cols <- 8
    numeric_vars <- c("ProjectId", "StationId", "Depth1", "Depth2")
    time_vars <- "SampleDate"
  } else if (sheetname == "SedimentChemistry"){ 
    number_of_key_cols <- 9
    numeric_vars <- c("ProjectId", "StationId", "SampleTag", "Depth1", "Depth2")
    time_vars <- "SampleDate"
  } else if (sheetname == "BiotaChemistry"){
    number_of_key_cols <- 12
    numeric_vars <- c("ProjectId", "StationId", "SampleId", "ReplicateNo")
    time_vars <- c("CatchDateFirst", "CatchDateLast", "SampleDate")
  }
  # Read key columns (leftmost columns)
  # Read column names as well (col_names = TRUE)
  df_key <- read_excel(
    fn, sheet = sheetname, col_names = TRUE, col_types = "text", 
    range = cell_limits(c(2,NA), c(NA, number_of_key_cols))
    )
  # Read substance names (row 1)
  # End cell = c(1, NA), where NA means number of columns is not set 
  df_names <- read_excel(
    fn, sheet = sheetname, col_names = FALSE, col_types = "text", 
    range = cell_limits(c(1, number_of_key_cols+1), c(1, NA))
  )
  # Set 'proper' column names "Substance_1", "Substance_2" etc.
  column_names <- paste0("Substance_", 1:ncol(df_names))
  names(df_names) <- column_names
  # Column names 
  # Read units (row 2)
  # Number of columns is set to be the same as df_names 
  df_units <- read_excel(
    fn, sheet = sheetname, col_names = names(df_names), col_types = "text", 
    range = cell_limits(c(2, number_of_key_cols+1), c(2, number_of_key_cols + ncol(df_names)))
  )
  # Read measurement data
  # All data are read as text (strings) and later converted, due to the "<" signs
  # Number of rows is set to be the same as df_keys 
  # Number of columns is set to be the same as df_names 
  df_chem <- read_excel(
    fn, sheet = sheetname, col_names = names(df_names), col_types = "text", 
    range = cell_limits(c(3, number_of_key_cols+1), c(nrow(df_key) + 2, number_of_key_cols + ncol(df_names)))
  )
  # df_key: convert these variables to numeric
  for (col in numeric_vars){
    df_key[[col]] <- as.numeric(df_key[[col]])
  }
  # df_key: convert these variables to time
  for (col in time_vars){
    df_key[[col]] <- lubridate::dmy_hms(df_key[[col]])
  }
  # Combine key variables and data, and put them on long format  
  data <- bind_cols(df_key, df_chem) %>%
    tidyr::pivot_longer(-seq(1,ncol(df_key)), names_to = "Temporary_name", values_to = "Value_chr")
  # Make look-up table for substance names
  X <- as.data.frame(df_names)
  lookup_names <- data.frame(
    Temporary_name = names(df_names),
    Substance = as.character(X[1,]),
    stringsAsFactors = FALSE
  )
  X <- as.data.frame(df_units)
  lookup_units <- data.frame(
    Temporary_name = names(df_units),
    Unit = as.character(X[1,]),
    stringsAsFactors = FALSE
  )
  # Add columns "Substance" and "Unit" to the data using the look-up tables
  data <- data %>% 
    filter(!is.na(Value_chr)) %>%
    left_join(lookup_names, by = "Temporary_name") %>%
    left_join(lookup_units, by = "Temporary_name")

  # Make numeric data values
  x <- sub(",", ".", data$Value_chr, fixed = TRUE)
  x <- sub(",", ".", x, fixed = TRUE)
  data$Value <- as.numeric(sub("<", "", x))
  
  # Make less-than flag
  data$Flag <- ifelse(grepl("<", data$Value_chr, fixed = TRUE), "<", NA)
  
  # Return the data, without the 'Temporary_name' column  
  data %>% select(-Temporary_name)
}