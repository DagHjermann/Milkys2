#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Generic function

search_lineno <- function(df, colno, text){
  sel <- df[[colno]] %in% text
  ifelse(sum(sel) > 0, which(sel)[1], NA)
}

# Test
test <- FALSE
if (test){
  testdata <- read.table(textConnection("
.            .     .  .
Site      Oslo     .  .    
Vessel Braarud     .  .
.            .     .  .
.            .     .  .
Year     Month  Salt  Flu
2005      1.00  32.0  0.525
2005      2.00  32.1  0.0286
2006      1.00  32.3  0.197
2006      2.00  32.3  0.107
2007      2.00  33.1  0.412
"), na = ".", header = TRUE)
  # Find where the actula data set starts
  search_lineno(testdata, 1, "Year")
}

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Function tailored to these data

read_fish_individual_data <- function(fn){
  df_try <- read_excel(paste0(datafolder, "/", fn)) %>% as.data.frame()
  
  # Find header of file
  line_header <- search_lineno(df_try, 1, "Fisknr")
  dat <- read_excel(paste0(datafolder, "/", fn), skip = line_header - 1)
  
  # If file has a hole (empty row) in the data, we delete data from the first hole
  col <- grep("Lengde", colnames(dat), ignore.case = TRUE)[1]
  first_empty <- which(is.na(dat[[col]]))[1]
  if (!is.na(first_empty))
    dat <- dat[1:(first_empty-1),]
  
  # Station
  row <- grep("Stasjon", df_try[[1]], ignore.case = TRUE)[1]
  station <- df_try[row, 3]

  # Species
  row <- grep("Art", df_try[[1]], ignore.case = TRUE)[1]
  species <- df_try[row, 3]

  dat <- dat %>%
    mutate(Station = station, 
           Species = species) %>%
    select(Station, Species, everything()) 
  
  dat
}

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Generic function

print_less_common_values <- function(txt){
  tab <- table(txt) %>% sort() %>% rev()
  original_options <- options(useFancyQuotes = FALSE)
  if (length(tab) > 1) {
    cat("Most common value:\n  ", sQuote(names(tab)[1]), " (", tab[1], " values)\n", sep = "")
    cat("Other values:\n")
    for (i in 2:length(tab)){
      no <- txt %in% names(tab)[i]
      cat("  ", sQuote(names(tab)[i]), ", ", tab[i], " values", sep = "")
      indices <- which(txt %in% names(tab)[i])
      cat(" (",  paste(indices, collapse = ", "), ")\n", sep = "")
    }
  } else {
      cat("Only value:\n  ", sQuote(names(tab)[1]), " (", tab[1], " values)\n", sep = "")
  }
  options(original_options)
  invisible(names(tab)[1]) 
}
# Test
# print_less_common_values(c("a", "b", "c", "c", rep("d", 6), "e"))
# print_less_common_values(rep("d", 6))
# # Returns the most common value, invisibly
# x <- print_less_common_values(c("a", "b", "c", "c", rep("d", 6), "e"))
# x
 
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Generic function

# Get columns in a more robust way (e.g., case-insensitive)  
#
# Columns are given the name used for searching (i.e., standardized names)
# If a column is not found, it is simply skipped - this doesn't make problems 
#    for bind_rows, though (but note that column number of the following variables will change)
# Messages may or may not be displayed
#
get_column_number <- function(df, colname){
  grep(colname, colnames(df), ignore.case = TRUE)[1]   # picks the first one, if there are more
  }
# get_column_number(dat_list[[1]], "lengde")

get_columns <- function(df, colnames, messages = TRUE){
  colno <- colnames %>% map_int(~get_column_number(df, .))
  notok <- is.na(colno)
  if (messages) {
    if (sum(notok) > 0){
      cat("Variables", paste(sQuote(colnames[notok]), collapse = ", "), "were not found\n")
    } else {
      cat("All variables found\n")
    }
  }
  colno <- colno[!notok]
  df <- df[colno]
  colnames(df) <- colnames[!notok]
  df
}

# debugonce(get_columns)
# get_columns(dat_list[[1]], c("lengde", "vekt"))
# get_columns(dat_list[[1]], c("Lengde", "vekt"))
# get_columns(dat_list[[1]], c("asdd", "lengde", "wwad"))

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Generic function

char2numeric <- function(df, colname){
  df[[colname]] <- as.numeric(sub(",", ".", df[[colname]]))
  df
}
# dat_list_selected[[3]]
# char2numeric(dat_list_selected[[3]], "Lengde")


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Generic function

# Column classes for a single data frame, given as a one-row data frame
# (so suitable for binding to a multi-row data frame using map_df)
get_column_classes <- function(df){
  result <- df %>% map_chr(class) %>% matrix(nrow = 1) %>% data.frame(stringsAsFactors = FALSE)
  colnames(result) <- colnames(df)
  result
}
# Test:
# get_column_classes(dat_list_selected[[1]])

