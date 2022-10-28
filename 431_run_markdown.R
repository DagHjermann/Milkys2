
library(dplyr)

# dir.create("431_reports")

lookup_eqs <- read.csv("Input_data/Lookup_tables/Lookup_EQS_limits.csv") %>%
  filter(Basis == "WW")

params_all <- c("AG", "AS", "CD", "CO", "CR", "CU", "HG-WW", "HG-WWa", "NI", "PB", "ZN", 
  "BDE47", "BDE99", "BDE100", "BDE153", 
  "HBCDA", "HBCDD", "CB118", 
  "CB138",  "CB153", 
  "PFOA", "PFOS", "PFOSA", 
  "ANT", "NAP", "BAA", "BAP", "FLU", "PYR", 
  "D5", 
  "HCB", "QCB", "HCHG", "DDEPP", 
  "TBT", "TPhT")


# stops in eqs plot:
# "DDEPP" 
#  - only one station
# D5 
# - 06_ratio_data_2 - Error in `left_join()`: ! Can't join on `x$STATION_CODE` x `y$STATION_CODE` because of incompatible type

not_working <- c("DDEPP", "D5", "HCHG", "TBT", "TPhT")
params_all_working <- params_all[!params_all %in% not_working]


# Ad hoq
# params1 <- c("AS", "CR", "NI", "ZN", "BDE99", "BDE153", "PYR", "D5")

# Get parameters already done:
# fns <- dir("Figures/431", pattern = "prorefratio_ord")
# params_finished <- stringr::str_extract(fns, "[^_]+")
# params1 <- setdiff(params_all_working, params1)

# stops:
# BDE47

# params_all_working <- c("HG-WW", "HG-WWa")

for (param in params_all_working){

  cat("****************************************\n", param, "\n****************************************\n")

  eqsplot <- as.character(param %in% lookup_eqs$PARAM)
  eqsplot
  
  if (param == "HG-WWa"){
    param2 <- "HG"
    basis_fish <- "WWa"
  } else if (param == "HG-WW"){
    param2 <- "HG"
    basis_fish <- "WW"
  } else {
    param2 <- param
    basis_fish <- "WW"
  }
  
  # Render HTML and .md files  
  rmarkdown::render(
    input = '431_Report_parameter_static.Rmd',          
    output_file = paste0('431_reports/Report_', param),                   # change here
    params = list(
      param = param2,                 # change here
      basis_fish = basis_fish,
      eqsplot = eqsplot)
  )
  
}


#
# Check number of <LOQ for last year for every parameter (i.e. which plots that should be updated)  
#

if (FALSE){
  
  dat_raw <- readRDS("Data/109_adjusted_data_2022-09-23.rds")                              
  
  for (param in params_all_working){
    
    param <- "BDE47"

    check <- dat_raw %>%
      filter(PARAM %in% params_all & MYEAR %in% 2021)
    xtabs(~ PARAM + is.na(FLAG1), check)
    
    xtabs(~ PARAM + is.na(FLAG1), check %>% filter(LATIN_NAME %in% c("Gadus morhua", "Somateria mollissima")))
    xtabs(~ PARAM + is.na(FLAG1), check %>% filter(LATIN_NAME %in% c("Mytilus edulis")))
    
    tab <- xtabs(~ PARAM + is.na(FLAG1) + LATIN_NAME, check)
    ftableable(PARAM ~ ., data = tab)
    # ftable(LATIN_NAME ~ ., data = tab)
    # ftable(is.na(FLAG1) ~ ., data = tab)
    

  }
  
  
}
