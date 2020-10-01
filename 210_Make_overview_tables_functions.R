#
# Defining NAme_english and Tissue_english used in tables 
#

df_common_name = data.frame(
  LATIN_NAME = c("Mytilus edulis", "Gadus morhua", "N. lapillus / L. littorea", "Nucella lapillus", "Somateria mollissima"),
  Name_english = c("Blue mussel", "Cod", "Dog whelk / periwinkle", "Dog whelk", "Eider duck"),
  stringsAsFactors = FALSE
)

df_tissue_name = data.frame(
  TISSUE_NAME = c("Whole soft body", "Lever", "Muskel", "Blod", "Egg homogenate of yolk and albumin"),
  Tissue_english = c("Soft body", "Liver", "Muscle", "Blood", "Egg"),
  stringsAsFactors = FALSE
)

#
# Make a list (one list element per station) where each list element = data frame with LATIN_NAME, PARAM, median
#   last yera, trend and EQS
#

get_summary_onestation <- function(station, stationname, species, tissue, par_table, data,
                                   varname_median, varname_trends){
  variable_median <- quo(varname_median)
  variable_trends <- quo(varname_trends)
  data_sel <- data %>%
    filter(STATION_CODE %in% station & LATIN_NAME %in% species &
             TISSUE_NAME %in% tissue & PARAM %in% par_table & 
             Basis %in% "WW" & !is.na(!!variable_median)) #%>%
  #  select(STATION_CODE, LATIN_NAME, TISSUE_NAME, PARAM, !!variable_median, !!variable_trends, EQS, Q95) %>%
  #  rename(Median_conc = !!variable_median, Trend = !!variable_trends)
  #data_sel <- left_join(data_sel, df_common_name,  by = "LATIN_NAME")
  #data_sel <- left_join(data_sel, df_tissue_name,  by = "TISSUE_NAME")
  #data_sel$STATION_NAME <- stationname
  data_sel
}

#
#
# For table 10 (Proref/change table)
#
#

write_excel_summarytable <- function(data_list, filename, param_names,
                                     cols = grey(seq(1, 0, length = 6)),
                                     varname_trend, varname_median,
                                     varname_proref = "Q95"){
  
  output_per_station <- FALSE 
  
  wb <- createWorkbook()
  sheet <- createSheet(wb, "Sheet1")
  linecount <- 1
  
  # Style for column headings
  cs_head <- CellStyle(wb) +
    Font(wb, heightInPoints=11, isBold=TRUE, isItalic=FALSE,
         name="Calibri", color="white")
  
  # Style for cells with medians < reference level
  cs_back <- CellStyle(wb) +
    DataFormat("0.0000") +
    Font(wb, heightInPoints=11, isBold=FALSE, isItalic=FALSE,
         name="Calibri", color="white") +
    Fill(backgroundColor = cols[1], foregroundColor = cols[1],
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_RIGHT")
  
  # Style for cells with medians over reference level, but below 2 x reference level
  cs_over1 <- CellStyle(wb) +
    DataFormat("0.0000") +
    Font(wb, heightInPoints=11, isBold=FALSE, isItalic=FALSE,
         name="Calibri", color="white") +
    Fill(backgroundColor = cols[2], foregroundColor = cols[2],
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_RIGHT")
  
  # Style for cells with medians over 2 x reference level, but below 5 x reference level
  cs_over2 <- CellStyle(wb) +
    DataFormat("0.0000") +
    Font(wb, heightInPoints=11, isBold=FALSE, isItalic=FALSE,
         name="Calibri", color="white") +
    Fill(backgroundColor = cols[3], foregroundColor = cols[3],
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_RIGHT")
  
  # Etc.    
  cs_over3 <- CellStyle(wb) +
    DataFormat("0.0000") +
    Font(wb, heightInPoints=11, isBold=FALSE, isItalic=FALSE,
         name="Calibri", color="white") +
    Fill(backgroundColor = cols[4], foregroundColor = cols[4],
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_RIGHT")
  
  cs_over4 <- CellStyle(wb) +
    DataFormat("0.0000") +
    Font(wb, heightInPoints=11, isBold=FALSE, isItalic=FALSE,
         name="OCR A Extended", color="black") +
    Fill(backgroundColor = cols[5], foregroundColor = cols[5],
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_RIGHT")
  
  cs_over5 <- CellStyle(wb) +
    DataFormat("0.0000") +
    Font(wb, heightInPoints=11, isBold=FALSE, isItalic=FALSE,
         name="OCR A Extended", color="black") +
    Fill(backgroundColor = cols[6], foregroundColor = cols[6],
         pattern="SOLID_FOREGROUND") +
    Alignment(h="ALIGN_RIGHT")    
  
  # logfile <- file(paste0(filename, "_log.txt"), "w+")                # Print i (index in 'background_levels' file) and j (station nr. j) to text file
  # cat(format(Sys.time(), "%a %b %d %H:%M:%S %Y"), file = logfile)
  
  # Set column names
  varnames <- c("Station", "Station name", "Species", "Tissue", param_names)
  k <- length(varnames) - length(param_names)
  rows <- createRow(sheet, rowIndex = )
  for (i in 1:length(varnames)){
    cell <- createCell(rows, colIndex = i)[[1,1]]
    setCellValue(cell, varnames[i])
    setCellStyle(cell, cs_head)
  }
  
  N <- length(data_list)
  
  for (i in 1:N){  # all
    if (output_per_station)
      cat("Data set ", i, "\n")
    linecount <- linecount + 1
    rows <- createRow(sheet, rowIndex=linecount)
    data <- data_list[[i]]
    cell <- createCell(rows, colIndex = 1)[[1,1]]
    setCellValue(cell, data[1, "STATION_CODE"])
    cell <- createCell(rows, colIndex = 2)[[1,1]]
    setCellValue(cell, data[1, "Station.Name"])       # was STATION_NAME
    cell <- createCell(rows, colIndex = 3)[[1,1]]
    setCellValue(cell, data[1, "Name_english"])
    cell <- createCell(rows, colIndex = 4)[[1,1]]
    setCellValue(cell, data[1, "TISSUE_NAME"])        # was Tissue_english
    for (j in 1:nrow(data)){
      # browser()
      param_no <- which(param_names %in% data[j, "PARAM"])
      col_no <- param_no + k
      cell <- createCell(rows, colIndex = col_no)[[1,1]]
      # Write cell value (trend)
      setCellValue(cell, data[j, varname_trend])
      # Set cell format (based 
      concentr <- data[j, varname_median]
      proref <- data[j, varname_proref]
      if (!is.na(proref) & !is.na(concentr)){
        if (concentr <= proref){
          setCellStyle(cell, cs_back)
        } else if (concentr > proref & concentr <= 2*proref) {
          setCellStyle(cell, cs_over1)
          setCellStyle(cell, cs_over1)
        } else if (concentr > 2*proref & concentr <= 5*proref) {
          setCellStyle(cell, cs_over2)
        } else if (concentr > 5*proref & concentr <= 10*proref) {
          setCellStyle(cell, cs_over3)
        } else if (concentr > 10*proref & concentr <= 20*proref) {
          setCellStyle(cell, cs_over4)
        } else if (concentr > 20*proref) {
          setCellStyle(cell, cs_over5)
        }
      }  
    }
  }
  saveWorkbook(wb, filename)
  # cat("\n\n")
  # cat(format(Sys.time(), "%a %b %d %H:%M:%S %Y"), file = logfile)
  # close(logfile)
  
}


#
#
# For table 11 (EQS table)
# As 'write_excel_summarytable_eqs', but writes greyscale background (for Proref) + red/green bullet (for EQS)
#
# Blue and red ("#92C5DE", "#B2182B") are from colorbrewer 8-class RdBu
#

write_excel_summarytable_proref_eqs <- function(data_list, filename, param_names,
                                                cols = c("#FFFFFF", "#EDEDED", "#AFAFAF", "#777777", "#4F4F4F", "#000000"),
                                                cols_eqs = c("#74add1", "#B2182B"),
                                                varname_median,
                                                varname_proref = "Q95",
                                                varname_eqs = "EQS"){
  wb <- createWorkbook()
  sheet <- createSheet(wb, "Sheet1")
  linecount <- 1
  
  # Style for column headings
  cs_head <- CellStyle(wb) +
    Font(wb, heightInPoints=11, isBold=TRUE, isItalic=FALSE, name="Calibri", color="white")
  
  # Style for cells with medians < reference level
  cs_back_eqs1 <- CellStyle(wb) +
    Fill(backgroundColor = cols[1], foregroundColor = cols[1], pattern="SOLID_FOREGROUND") +  
    Font(wb, heightInPoints = 11, isBold=FALSE, isItalic=FALSE, name="Webdings", color = cols_eqs[1])
  cs_back_eqs2 <- CellStyle(wb) +
    Fill(backgroundColor = cols[1], foregroundColor = cols[1], pattern="SOLID_FOREGROUND") +  
    Font(wb, heightInPoints = 11, isBold=FALSE, isItalic=FALSE, name="Webdings", color = cols_eqs[2])
  
  # Style for cells with medians over reference level, but below 2 x reference level
  cs_over1_eqs1 <- CellStyle(wb) +
    Fill(backgroundColor = cols[2], foregroundColor = cols[2], pattern="SOLID_FOREGROUND") +  
    Font(wb, heightInPoints = 11, isBold=FALSE, isItalic=FALSE, name="Webdings", color = cols_eqs[1])
  cs_over1_eqs2 <- CellStyle(wb) +
    Fill(backgroundColor = cols[2], foregroundColor = cols[2], pattern="SOLID_FOREGROUND") +  
    Font(wb, heightInPoints = 11, isBold=FALSE, isItalic=FALSE, name="Webdings", color = cols_eqs[2])
  
  # Style for cells with medians over 2 x reference level, but below 5 x reference level
  cs_over2_eqs1 <- CellStyle(wb) +
    Fill(backgroundColor = cols[3], foregroundColor = cols[3], pattern="SOLID_FOREGROUND") +  
    Font(wb, heightInPoints = 11, isBold=FALSE, isItalic=FALSE, name="Webdings", color = cols_eqs[1])
  cs_over2_eqs2 <- CellStyle(wb) +
    Fill(backgroundColor = cols[3], foregroundColor = cols[3], pattern="SOLID_FOREGROUND") +  
    Font(wb, heightInPoints = 11, isBold=FALSE, isItalic=FALSE, name="Webdings", color = cols_eqs[2])
  
  # Etc.    
  cs_over3_eqs1 <- CellStyle(wb) +
    Fill(backgroundColor = cols[4], foregroundColor = cols[4], pattern="SOLID_FOREGROUND") +  
    Font(wb, heightInPoints = 11, isBold=FALSE, isItalic=FALSE, name="Webdings", color = cols_eqs[1])
  cs_over3_eqs2 <- CellStyle(wb) +
    Fill(backgroundColor = cols[4], foregroundColor = cols[4], pattern="SOLID_FOREGROUND") +  
    Font(wb, heightInPoints = 11, isBold=FALSE, isItalic=FALSE, name="Webdings", color = cols_eqs[2])
  
  cs_over4_eqs1 <- CellStyle(wb) +
    Fill(backgroundColor = cols[5], foregroundColor = cols[5], pattern="SOLID_FOREGROUND") +  
    Font(wb, heightInPoints = 11, isBold=FALSE, isItalic=FALSE, name="Webdings", color = cols_eqs[1])
  cs_over4_eqs2 <- CellStyle(wb) +
    Fill(backgroundColor = cols[5], foregroundColor = cols[5], pattern="SOLID_FOREGROUND") +  
    Font(wb, heightInPoints = 11, isBold=FALSE, isItalic=FALSE, name="Webdings", color = cols_eqs[2])
  
  cs_over5_eqs1 <- CellStyle(wb) +
    Fill(backgroundColor = cols[6], foregroundColor = cols[6], pattern="SOLID_FOREGROUND") +  
    Font(wb, heightInPoints = 11, isBold=FALSE, isItalic=FALSE, name="Webdings", color = cols_eqs[1])
  cs_over5_eqs2 <- CellStyle(wb) +
    Fill(backgroundColor = cols[6], foregroundColor = cols[6], pattern="SOLID_FOREGROUND") +  
    Font(wb, heightInPoints = 11, isBold=FALSE, isItalic=FALSE, name="Webdings", color = cols_eqs[2])
  
  # logfile <- file(paste0(filename, "_log.txt"), "w+")                # Print i (index in 'background_levels' file) and j (station nr. j) to text file
  # cat(format(Sys.time(), "%a %b %d %H:%M:%S %Y"), file = logfile)
  
  # Set column names
  varnames <- c("Station", "Station name", "Species", param_names)
  k <- length(varnames) - length(param_names)
  rows <- createRow(sheet, rowIndex = )
  for (i in 1:length(varnames)){
    cell <- createCell(rows, colIndex = i)[[1,1]]
    setCellValue(cell, varnames[i])
    setCellStyle(cell, cs_head)
  }
  
  N <- length(data_list)
  
  for (i in 1:N){  # all
    linecount <- linecount + 1
    rows <- createRow(sheet, rowIndex=linecount)
    data <- data_list[[i]]
    cell <- createCell(rows, colIndex = 1)[[1,1]]
    setCellValue(cell, data[1, "STATION_CODE"])
    cell <- createCell(rows, colIndex = 2)[[1,1]]
    setCellValue(cell, data[1, "Station.Name"])     # was STATION_NAME
    cell <- createCell(rows, colIndex = 3)[[1,1]]
    setCellValue(cell, data[1, "Name_english"])
    for (j in 1:nrow(data)){
      # if (j == 6) browser()
      # if (data[j, "PARAM"] == "CB_S7") browser()
      param_no <- which(param_names %in% data[j, "PARAM"])
      # Make cell
      col_no <- param_no + k
      cell <- createCell(rows, colIndex = col_no)[[1,1]]
      # Set cell background format based on Proref
      concentr <- data[j, varname_median]
      proref <- data[j, varname_proref]
      EQS <- data[j, varname_eqs]
      # Format based on median relative to proref + EQS 
      if (is.na(proref))
        proref <- 1E9
      if (!is.na(concentr) & !is.na(EQS)){
        # EQS symbol: we use letter 'n', in Webdings font that becomes a 'bullet'
        setCellValue(cell, "n")
        if (concentr <= proref & concentr <= EQS){
          setCellStyle(cell, cs_back_eqs1)
        } else if (concentr <= proref & concentr > EQS){
          setCellStyle(cell, cs_back_eqs2)
        } else if (concentr > proref & concentr <= 2*proref & concentr <= EQS) {
          setCellStyle(cell, cs_over1_eqs1)
        } else if (concentr > proref & concentr <= 2*proref & concentr > EQS) {
          setCellStyle(cell, cs_over1_eqs2)
        } else if (concentr > 2*proref & concentr <= 5*proref & concentr <= EQS) {
          setCellStyle(cell, cs_over2_eqs1)
        } else if (concentr > 2*proref & concentr <= 5*proref & concentr > EQS) {
          setCellStyle(cell, cs_over2_eqs2)
        } else if (concentr > 5*proref & concentr <= 10*proref & concentr <= EQS) {
          setCellStyle(cell, cs_over3_eqs1)
        } else if (concentr > 5*proref & concentr <= 10*proref & concentr > EQS) {
          setCellStyle(cell, cs_over3_eqs2)
        } else if (concentr > 10*proref & concentr <= 20*proref & concentr <= EQS) {
          setCellStyle(cell, cs_over4_eqs1)
        } else if (concentr > 10*proref & concentr <= 20*proref & concentr > EQS) {
          setCellStyle(cell, cs_over4_eqs2)
        } else if (concentr > 20*proref & concentr <= EQS) {
          setCellStyle(cell, cs_over5_eqs1)
        } else if (concentr > 20*proref & concentr > EQS) {
          setCellStyle(cell, cs_over5_eqs2)
        }
      }
    }  # for each j
  }  # for each i
  saveWorkbook(wb, filename)
  # cat("\n\n")
  # cat(format(Sys.time(), "%a %b %d %H:%M:%S %Y"), file = logfile)
  # close(logfile)
  
}
