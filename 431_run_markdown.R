
library(dplyr)

# dir.create("431_reports")

lookup_eqs <- read.csv("Input_data/Lookup_tables/Lookup_EQS_limits.csv") %>%
  filter(Basis == "WW")

params_all <- c("AG", "AS", "CD", "CO", "CR", "CU", "HG", "NI", "PB", "ZN", 
  "BDE47", "BDE99", "BDE100", "BDE153", "HBCDA", "CB118", "CB138",  "CB153", 
  "PFOA", "PFOS", "PFOSA", "BAA", "BAP", "FLU", "PYR", "D5", "HCB")


# stops in eqs plot:
# "DDEPP" 
#  - only one station
# D5 
# - 06_ratio_data_2 - Error in `left_join()`: ! Can't join on `x$STATION_CODE` x `y$STATION_CODE` because of incompatible type

params_all_working <- params_all[!params_all %in% c("DDEPP", "D5")]


# Ad hoq
# params1 <- c("AS", "CR", "NI", "ZN", "BDE99", "BDE153", "PYR", "D5")

# Get parameters already done:
# fns <- dir("Figures/431", pattern = "prorefratio_ord")
# params_finished <- stringr::str_extract(fns, "[^_]+")
# params1 <- setdiff(params_all_working, params1)

# stops:
# BDE47


for (param in params_all_working){

  cat("****************************************\n", param, "\n****************************************\n")

  eqsplot <- as.character(param %in% lookup_eqs$PARAM)
  eqsplot
  
  # Render HTML and .md files  
  rmarkdown::render(
    input = '431_Report_parameter_static.Rmd',          
    output_file = paste0('431_reports/Report_', param),                   # change here
    params = list(
      param = param,                 # change here
      eqsplot = eqsplot)
  )
  
}


