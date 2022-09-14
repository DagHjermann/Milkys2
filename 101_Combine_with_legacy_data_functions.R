
sum_parameters <- list(
  CB_S7 = c("CB28", "CB52", "CB101", "CB118", "CB138", "CB153", "CB180"), 
  CB_S6 = c("CB28", "CB52", "CB101", "CB138", "CB153", "CB180"), 
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
# Set standard parameternames based on NAME given in NIVAbasen (METHOD_DEFINITIONS)   
# 'synonymfile' should have the 'standard' name (the name you want to change to) in the first column
#    and the other names from column 2 on
#
get_standard_parametername <- function(x, synonymfile){
  if (is.factor(x))
    x <- levels(x)[as.numeric(x)]
  synonyms <- read.csv(synonymfile, stringsAsFactors = FALSE)
  n_cols <- ncol(synonyms)
  # will search in all columns named "substance"
  cols_synonyms <- grep("substance", colnames(synonyms))  # returns number
  # except the first one
  cols_synonyms <- cols_synonyms[cols_synonyms > 1]
  # note that number of synonyms is 7, must be changed if file is changed!
  for (col in cols_synonyms){
    for (row in seq_len(nrow(synonyms))){
      sel <- x %in% synonyms[row,col]
      x[sel] <- synonyms[row, 1]
    }
  }
  x
}
# Example
# get_standard_parametername(c("Benzo[a]pyren", "Benzo[b,j]fluoranten"), "Input_data/Lookup table - standard parameter names.csv")


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
      summarise(VALUE = sum(VALUE, na.rm = TRUE), .groups = "drop_last") %>%      # sum of the measurements
      mutate(QUANTIFICATION_LIMIT = NA) %>%
      as.data.frame(stringsAsFactors = FALSE)
    df2 <- df_grouped %>%
      summarise(FLAG1 = ifelse(mean(!is.na(FLAG1))==1, "<", as.character(NA)), 
                .groups = "drop_last") %>%       # If all FLAG1 are "<", FLAG1 = "<", otherwise FLAG1 = NA
      as.data.frame()
    df2$FLAG1[df2$FLAG1 %in% "NA"] <- NA
    df3 <- df_grouped %>%
      summarise(N_par = n(), .groups = "drop_last") %>%    # number of measurements
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

# In contrast to add_sumparameter (which is only for last year's data), this is for use on entire data series (dat_updated2) 
# - for all columns VALUE_WW, VALUE_DW, VALUE_FB, VALUE_WWa, VALUE_DWa, VALUE_FBa
# - extra columns DRYWT, FAT_PERC, LNMEA, STATION_NAME, SAMPLE_DATE (we drop UNCERTAINTY, QUANTIFICATION_LIMIT etc.)

add_sumparameter_exloq <- function(i, pars_list, data){
  # Add variable N_par, if it's not already there
  if (!"N_par" %in% colnames(data)){
    data$N_par <- 1
  }
  pars <- pars_list[[i]]
  cat("==================================================================\n", i, names(pars_list)[i], "\n")
  cat(pars, "\n")
  # Reshape data (from VALUE_WW, VALUE_DW, etc.  to VALUE and BASIS columns)
  df_reshaped <- data %>%
    filter(PARAM %in% pars & !is.na(SAMPLE_NO2)) %>%                        # select records (only those with SAMPLE_NO2)
    select(
      STATION_CODE, LATIN_NAME, TISSUE_NAME, MYEAR, SAMPLE_NO2, UNIT, 
      FLAG1, VALUE_WW, VALUE_DW, VALUE_FB, VALUE_WWa, VALUE_DWa, VALUE_FBa) %>%
    tidyr::pivot_longer(
      cols = c(VALUE_WW, VALUE_DW, VALUE_FB, VALUE_WWa, VALUE_DWa, VALUE_FBa), names_to = "BASIS", values_to = "VALUE") %>%
    mutate(
      VALUE_exloq = case_when(
        is.na(FLAG1) ~ VALUE,
        !is.na(FLAG1) ~ 0)
    )
  df_grouped <- df_reshaped %>%
    group_by(STATION_CODE, LATIN_NAME, TISSUE_NAME, MYEAR, SAMPLE_NO2, UNIT, BASIS) # not PARAM, but including BASIS
  if (nrow(df_grouped) > 0){
    df1 <- df_grouped %>%
      summarise(VALUE = sum(VALUE_exloq, na.rm = TRUE), .groups = "drop_last") %>%      # sum of the measurements
      mutate(QUANTIFICATION_LIMIT = NA) %>%
      as.data.frame(stringsAsFactors = FALSE)
    df3 <- df_grouped %>%
      summarise(N_par = n(), .groups = "drop_last") %>%    # number of measurements
      as.data.frame()
    # Should be all 1
    # check <- df1[,1:9] == df2[,1:9]
    # cat("Test 1 (should be 1):", 
    #     apply(check, 2, mean) %>% mean(na.rm = TRUE), "\n")
    # 
    # check <- df2[,1:9] == df3[,1:9]
    # cat("Test 2 (should be 1):", 
    #     apply(check, 2, mean) %>% mean(na.rm = TRUE), "\n")
    
    # Set the parameter name
    df1$PARAM <- paste0(names(pars_list)[i], "_exloq")   
    
    df_to_add1 <- data.frame(df1, FLAG1 = as.character(NA), N_par = df3[,"N_par"], stringsAsFactors = FALSE)  # Make data to add
    
    # Get and add support parameters
    df_supportpars <- data %>%
      filter(PARAM %in% pars & !is.na(SAMPLE_NO2)) %>%  
      group_by(STATION_CODE, LATIN_NAME, TISSUE_NAME, MYEAR, SAMPLE_NO2, UNIT) %>%
      summarise(
        DRYWT = mean(DRYWT, na.rm = TRUE),
        FAT_PERC = mean(FAT_PERC, na.rm = TRUE),
        LNMEA = mean(LNMEA, na.rm = TRUE),
        STATION_NAME = first(STATION_NAME),
        SAMPLE_DATE = first(SAMPLE_DATE), .groups = "drop")
    df_to_add2 <- df_to_add1 %>%
      left_join(df_supportpars, by = c("STATION_CODE", "LATIN_NAME", "TISSUE_NAME", "MYEAR", "SAMPLE_NO2", "UNIT"))
    
    # Reshape data (get back columns VALUE_WW, VALUE_DW, etc.  )
    df_to_add_reshaped <- df_to_add2 %>%
      tidyr::pivot_wider(names_from = BASIS, values_from = VALUE)
    data <- bind_rows(data, df_to_add_reshaped)   # Add data for this parameter
    cat("Number of rows added:", nrow(df_to_add_reshaped), "; number of rows in data:", nrow(data), "\n")
  } else {
    cat("No rows added (found no data for these parameters)\n")
  }
  data
}


# Summarises a sequence into a string, e.g. "1991-1993,1996-1997,2000"
summarize_sequence <- function(x){
  x <- sort(unique(x))
  dx <- diff(x)
  df <- tibble(
    x = x,
    index = cumsum(c(1, dx) > 1) + 1)
  df %>% 
    group_by(index) %>%
    summarize(Min = min(x),Max = max(x),  .groups = "drop") %>%
    mutate(Summ = ifelse(Min < Max, paste0(Min,"-",Max), Min)) %>%
    summarize(Summ = paste0(Summ, collapse = ","), .groups = "drop") %>%
    pull(Summ)
}

# Summarizes which samples we have for which parameters, returns table  
summarize_samples <- function(data) {
  data %>%
    count(PARAM, SAMPLE_NO2) %>%
    group_by(PARAM) %>%
    summarize(
      Samples = summarize_sequence(which(n > 0)), 
      Samples_n = sum(n > 0), 
      .groups = "drop") %>%
    group_by(Samples) %>%
    summarize(
      PARAM = paste(PARAM, collapse = ", "),
      No_of_samples = first(Samples_n),
      .groups = "drop") %>%
    arrange(No_of_samples)
}

# Example
if (FALSE){
  dat_new6 %>% 
    filter(TISSUE_NAME %in% "Lever" & MYEAR == 2019 & STATION_CODE == "30B") %>%
    summarize_samples() %>%
    View()
  
}

# Summarizes which samples we have for which parameters, returns printout  
summarize_samples_print <- function(data) {
  df <- summarize_samples(data)
  for (i in 1:nrow(df)){
    cat("Samples", df$Samples[i], "(", df$No_of_samples[i], "): \n")
    cat("    ", df$PARAM[i], " \n")
  }
}

# Example
if (FALSE){
  dat_new6 %>% 
    filter(TISSUE_NAME %in% "Lever" & MYEAR == 2019 & STATION_CODE == "30B") %>%
    summarize_samples_print()
  
}


