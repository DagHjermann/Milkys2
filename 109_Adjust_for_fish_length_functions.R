#
# lengthreg() - just doing the regression and plotting Value vs length 
# Input: data and selection (boolean vector)
#   Optionally: variable to adjust, and a (offset in log-transform)
# Output: Summary (vector of 6 numbers)
#
lengthreg <- function(data, selection, var = "VALUE_WW", a = 0){
  df <- data[selection, ] %>% as.data.frame()
  df$YEAR_f <- as.factor(df$MYEAR)
  df$VALUE <- df[,var]
  df$Seq <- 1:nrow(df)
  # Less-thans (data under LOQ) are set to 1/2 of LOQ
  sel_flag <- !is.na(df$FLAG1)
  df$VALUE[sel_flag] <- df$VALUE[sel_flag]*0.5
  # Doing the regression
  df$logVALUE <- log(df$VALUE + a)
  gg <- ggplot(df, aes(LNMEA, logVALUE)) + geom_point() + geom_smooth(method = "lm", color = 2) + facet_wrap(~YEAR_f)
  print(gg)
  mod <- lm(logVALUE ~ LNMEA + YEAR_f, data = df)
  mod_summary <- c(summary(mod)$coef["LNMEA", c(1,2,4)], N = nrow(df), N_reg = length(mod$residuals), a = a)
  mod_summary
}

# TEST
if (FALSE){
  sp <- "Gadus morhua"
  tis <- "Muskel"
  par <- "HG"
  st <- "30B"
  sel <- with(data_all, is.finite(VALUE_WW) & is.finite(LNMEA) & PARAM %in% par & LATIN_NAME %in% sp & TISSUE_NAME %in% tis & STATION_CODE %in% st)
  # debugonce(lengthreg)
  lengthreg(data_all, sel)
}

#
# Input: As lengthreg(), but in addition 'standard_length'
# Output: list of
#   value = vector
#   summary (as lengthreg)
#   'flag' (always true, added just to be 'compatible' with adjust_selected2 - which at the moment is not used)
#
adjust_selected <- function(data, selection, standard_length = 500, var = "VALUE_WW", a = 0, log = TRUE){
  df <- data[selection, ] %>% as.data.frame()
  df$YEAR_f <- as.factor(df$MYEAR)
  df$VALUE <- df[,var]
  # Less-thans (data under LOQ) are set to 1/2 of LOQ
  sel_flag <- !is.na(df$FLAG1)
  df$VALUE[sel_flag] <- df$VALUE[sel_flag]*0.5
  # Doing the regression
  if (log){
    df$logVALUE <- log(df$VALUE + a)
    df$logVALUE[(df$VALUE + a) <= 0] <- NA
    mod <- lm(logVALUE ~ LNMEA + YEAR_f, data = df)
    mod_summary <- c(summary(mod)$coef["LNMEA", c(1,2,4)], 
                     N = nrow(df), N_reg = length(mod$residuals), a = a, log = as.numeric(log))
    df_adjusted <- df
    df_adjusted$LNMEA <- standard_length
    result <- list(value = exp(predict(mod, df_adjusted) + resid(mod)) - a, 
                   summary = mod_summary, flag = rep(TRUE, nrow(df)))
  } else {
    mod <- lm(VALUE ~ LNMEA + YEAR_f, data = df)
    mod_summary <- c(summary(mod)$coef["LNMEA", c(1,2,4)], 
                     N = nrow(df), N_reg = length(mod$residuals), a = a, log = as.numeric(log))
    df_adjusted <- df
    df_adjusted$LNMEA <- standard_length
    result <- list(value = predict(mod, df_adjusted) + resid(mod) - a, 
                   summary = mod_summary, flag = rep(TRUE, nrow(df)))
  }
}


# TEST
if (FALSE){
  sp <- "Gadus morhua"
  tis <- "Muskel"
  par <- "HG"
  st <- "30B"
  sel <- with(data_all, is.finite(VALUE_WW) & is.finite(LNMEA) & PARAM %in% par & LATIN_NAME %in% sp & TISSUE_NAME %in% tis & STATION_CODE %in% st)
  X <- adjust_selected(data_all, sel)
  df <- data_all[sel,] %>% as.data.frame()
  df$VALUE_WW_adj <- X$value
  df2 <- df %>%
    select(MYEAR, LNMEA, VALUE_WW, VALUE_WW_adj) %>%
    gather("Type", "log_concentration", VALUE_WW, VALUE_WW_adj)
  ggplot(df2 %>% filter(MYEAR >= 2003), aes(LNMEA, log_concentration, color = Type)) + 
    geom_smooth(method = "lm") + geom_point() + 
    facet_wrap(~MYEAR)
}




#
# SUM PARAMETERS
#

#
# Building on the function of the same name in script 01...functions, but simplified
#
add_sumparameter <- function(i, pars_list, data){
  # Add variable N_par, if it's not already there
  if (!"N_par" %in% colnames(data)){
    data$N_par <- 1
  }
  pars <- pars_list[[i]]
  cat("===========\n", i, names(pars_list)[i], "\n")
  cat(pars, "\n")
  df_to_add <- data %>%
    filter(PARAM %in% pars & !is.na(SAMPLE_NO2)) %>%                              # select records (only those with SAMPLE_NO2)
    group_by(STATION_CODE, LATIN_NAME, TISSUE_NAME, MYEAR, SAMPLE_NO2, UNIT) %>%  # not PARAM
    summarise(VALUE_WW = sum(VALUE_WW, na.rm = TRUE),                     # sum of the measurements
              VALUE_DW = sum(VALUE_DW, na.rm = TRUE),
              VALUE_FB = sum(VALUE_FB, na.rm = TRUE),
              FLAG1 = ifelse(mean(!is.na(FLAG1))==1, "<", as.character(NA)),  # If all FLAG1 are "<", FLAG1 = "<", otherwise FLAG1 = NA
              DRYWT = mean(DRYWT, na.rm = TRUE),
              FAT_PERC = mean(FAT_PERC, na.rm = TRUE),
              LNMEA = mean(LNMEA, na.rm = TRUE),
              N_par = n()
    ) %>%
    mutate(PARAM = names(pars_list)[i])
  
  df_orig <- data %>%
    select(STATION_CODE, LATIN_NAME, TISSUE_NAME, MYEAR, SAMPLE_NO2, 
           UNIT, PARAM, 
           DRYWT, FAT_PERC, LNMEA,
           VALUE_WW, VALUE_DW, VALUE_FB, FLAG1, N_par)
  data <- bind_rows(df_orig, df_to_add)   # Add data for this parameter
  cat("Number of rows added:", nrow(df_to_add), "; number of rows in data:", nrow(data), "\n")
  data
}

get_sumparameter_definitions <- function(synonymfile){
  synonyms <- read.csv2(synonymfile, stringsAsFactors = FALSE)
  pars_list <- vector("list", 8)
  for (i in 1:5){
    sumpar <- c("CB_S7", "BDE6S", "P_S", "PFAS", "HBCDD")[i]
    pars_list[[i]] <- synonyms$substances2[synonyms$Sums1 %in% sumpar]
  }
  pars_list[[6]] <- grep("^BDE", synonyms$substances2, value = TRUE)   
  pars_list[[7]] <- synonyms$substances2[synonyms$Sums1 %in% c("P_S","PAH16")]
  pars_list[[8]] <- synonyms$substances2[synonyms$IARC_class %in% c("1","2A","2B")]
  pars_list[[9]] <- c("DDEPP", "DDTPP")
  names(pars_list) <- c("CB_S7", "BDE6S", "P_S", "PFAS", "HBCDD", 
                        "BDESS", "PAH16", "KPAH", "DDTEP")
  pars_list
}
