
# Fixing data for time series, so we achieve one longer data series instead of two (or more) shorter ones   

# Very much hard-coded!

# Change STATION_CODE, in order to make a single time series for data with different STATION_CODE that in practice should count as the same station   
# * Fixing station 227G1 and 227G2 (shall count as 227G)  
# * Fixing station 36A and 36A1 (36A1 shall count as 36A)  
# 
# Also combines PARAM = VDSI and PARAM = Intersex to PARAM = "VDSI/Intersex" for station 71G  

homogenize_series <- function(data){
  
  cat("Fixing station 227G1 and 227G2 (shall count as 227G) \n") 
  sel <- data$STATION_CODE %in% c("227G1","227G2")
  data$STATION_CODE[sel] <- "227G"
  cat("- changed STATION_CODE for", sum(sel), "rows of data \n\n") 

  cat("Fixing station 36A and 36A1 (36A1 shall count as 36A) \n") 
  sel <- data$STATION_CODE %in% "36A1"; sum(sel)
  # xtabs(~MYEAR, data[sel,])
  data$STATION_CODE[sel] <- "36A"
  cat("- changed STATION_CODE for", sum(sel), "rows of data \n\n") 
  
  cat("Fix VDSI and Intersex at station 71G (both shall count as PARAM = 'VDSI/Intersex') \n")   
  sel <- with(data,
              STATION_CODE %in% "71G" & PARAM %in% c("VDSI","Intersex"))
  # xtabs(~MYEAR + PARAM, data[sel,])
  data$PARAM[sel] <- "VDSI/Intersex"
  data$LATIN_NAME[sel] <- "N. lapillus / L. littorea"
  cat("- changed PARAM for", sum(sel), "rows of data \n\n") 
  
  data
  
}
