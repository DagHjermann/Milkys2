
sum_parameters <- list(
  CB_S7 = c("CB28", "CB52", "CB101", "CB118", "CB138", "CB153", "CB180"), 
  BDE6S = c("BDE28", "BDE47", "BDE99", "BDE100", "BDE153", "BDE154"), 
  P_S = c("ACNLE", "ACNE", "FLE", "PA", "ANT", "FLU", 
          "PYR", "BAA", "CHR", "BBJF", "BKF", "BAP", "DBA3A", "BGHIP", 
          "ICDP", "BBJKF"), 
  PFAS = c("PFOS", "PFOSA"), 
  HBCDD = c("HBCDA", "HBCDB", "HBCDG"), 
  BDESS = c("BDE17", "BDE28", "BDE47", "BDE49", 
            "BDE66", "BDE71", "BDE77", "BDE85", "BDE99", "BDE100", "BDE119", 
            "BDE126", "BDE138", "BDE153", "BDE154", "BDE156", "BDE183", "BDE184", 
            "BDE191", "BDE196", "BDE197", "BDE205", "BDE206", "BDE207", "BDE209"), 
  PAH16 = c("ACNLE", "ACNE", "FLE", "PA", "ANT", "FLU", "PYR", 
            "BAA", "CHR", "BBJF", "BKF", "BAP", "DBA3A", "BGHIP", "ICDP", 
            "NAP", "BBJKF"), 
  KPAH = c("BAA", "CHR", "BBJF", "BKF", "BAP", 
           "DBA3A", "ICDP"), 
  DDTEP = c("DDEPP", "DDTPP")
)


# 
# Adds a sum parameter as new rows to the data
#
# Takes 'data' as input, calculates sum parameter number 'i' from 'pars_list', and returnes 'data' with new rows added to it
#
add_sumparameter <- function(i, pars_list, data){
  # Add variable N_par, if it's not already there
  if (!"N_par" %in% colnames(data)){
    data$N_par <- 1
  }
  pars <- pars_list[[i]]
  cat("==================================================================\n", i, names(pars_list)[i], "\n")
  cat(pars, "\n")
  df_grouped <- data %>%
    filter(PARAM %in% pars & !is.na(SAMPLE_NO2)) %>%                        # select records (only those with SAMPLE_NO2)
    group_by(STATION_CODE, LATIN_NAME, TISSUE_NAME, MYEAR, SAMPLE_NO2, BASIS, UNIT)  # not PARAM
  if (nrow(df_grouped) > 0){
    df1 <- df_grouped %>%
      summarise(VALUE = sum(VALUE, na.rm = TRUE)) %>%      # sum of the measurements
      mutate(QUANTIFICATION_LIMIT = NA) %>%
      as.data.frame(stringsAsFactors = FALSE)
    df2 <- df_grouped %>%
      summarise(FLAG1 = ifelse(mean(!is.na(FLAG1))==1, "<", as.character(NA))) %>%       # If all FLAG1 are "<", FLAG1 = "<", otherwise FLAG1 = NA
      as.data.frame()
    df2$FLAG1[df2$FLAG1 %in% "NA"] <- NA
    df3 <- df_grouped %>%
      summarise(N_par = n()) %>%    # number of measurements
      as.data.frame()
    # Should be all 1
    # check <- df1[,1:9] == df2[,1:9]
    # cat("Test 1 (should be 1):", 
    #     apply(check, 2, mean) %>% mean(na.rm = TRUE), "\n")
    # 
    # check <- df2[,1:9] == df3[,1:9]
    # cat("Test 2 (should be 1):", 
    #     apply(check, 2, mean) %>% mean(na.rm = TRUE), "\n")
    
    # Change the parameter name
    df1$PARAM <- names(pars_list)[i]   
    
    df_to_add <- data.frame(df1, FLAG1 = df2[,"FLAG1"], N_par = df3[,"N_par"], stringsAsFactors = FALSE)  # Make data to add
    data <- bind_rows(data, df_to_add)   # Add data for this parameter
    cat("Number of rows added:", nrow(df_to_add), "; number of rows in data:", nrow(data), "\n")
  } else {
    cat("No rows added (found no data for these parameters)\n")
  }
  data
}

