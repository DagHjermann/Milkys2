##########################################################################################################################
# 36D fix_utf8                 Reading a UTF-8 file using default Windows-1252 encoding, changes for instance "ø" to strange "characters" such as "Ã¸" 
#                              This function attempts to change them to UTF-8 characters (such as "ø") 
#                              NOTE: depends on an external file, 'utf8_from_windows1252_4.txt'
#                              The table was copied/modified from http://www.i18nqa.com/debug/utf8-debug.html
#                              Note that it won't succeed with all characters, for instance several UTF-8 characters are changed to "Ã" by Windows-1252
#                                "Ã¿" can be both "Ø" and "Å".
#                                So it is always better to set UTF-8 encoding when reading the file (encoding = "UTF-8" inside e.g. read.csv).
#                                However, this doesn't seem to be possible with e.g., xmlTreeParse() so then this function is the last resort.
#                              But also see
#                                http://stackoverflow.com/questions/16561702/parsing-xml-attribute-strange-encoding-issue?rq=1
#                                https://dom.hastin.gs/files/utf8/
##########################################################################################################################

fix_utf8 <- function(string_vector){
  df_utf8 <- read.delim("Input_data/utf8_from_windows1252_4.txt", sep = "\t", encoding = "UTF-8", header = FALSE, stringsAsFactors = FALSE)
  df_utf8 <- df_utf8[order(nchar(df_utf8[,3]), decreasing = TRUE),]   # sort so that all 2-character windows1252 "characters" comes first, 
  #   otherwise "Ã" will be found and replaced before "Ã¸"
  for (i in 1:nrow(df_utf8)){
    # cat(i, "", rownames(year_range)[2], "\n")
    string_vector <- gsub(df_utf8[i,3], df_utf8[i,2], string_vector)
  }
  string_vector
}

# df_stat$VAnnforekomstNavn[1:100]
# df_stat$VAnnforekomstNavn[1:100] %>% fix_utf8()
