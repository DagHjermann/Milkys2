#
# For runnign as "background job" in RStudio
#
# Before running this, you must have defined 'folder_results', 'folder_input', 'dat_series_trend' *with the series you will run*
#   and 'dat_all_prep3'. Run script 125 parts 0, 1 and the "read statements' in part 2i.
#
# Then run job "with copy of local environment"
#

# General purpose
library(dplyr)
library(tidyr)
library(purrr)
library(mgcv)    #  mgcv_1.8-39
# install.packages("mgcv")  # for mgcv_1.8-40, b
# packageVersion("mgcv")
packageVersion("mgcv")
library(ggplot2)

# Specific for the analysis
library(rjags)
library(runjags)
if (!"leftcensored" %in% installed.packages())
  devtools::install_github("DagHjermann/leftcensored", upgrade = "never", force = TRUE)
library(leftcensored)

# For parallel computing
if (!"doParallel" %in% installed.packages())
  install.packages("doParallel")
library(doParallel)

# Load functions defined in other scripts  
# NOT NEEDED


# 4 / 16 / 64 
cl <- makeCluster(2)
registerDoParallel(cores = 2)


#
# Parallel 
#

# OLD: seriesno = row number
# seriesno <- 1:nrow(dat_series_trend) 

# NEW: seriesno = column in the 'dat_series' data
series_no <- dat_series_trend$series_no 

range(series_no)
length(series_no) # 1507

# Note folder name!

t0 <- Sys.time()
result <- foreach(i = series_no, 
                  .export = c("dat_all_prep3","dat_series_trend")) %dopar%
  get_splines_results_seriesno_s(i, 
                                 dat_all_prep3, dat_series_trend, foldername = folder_results,
                                 raftery = TRUE)
t1 <- Sys.time()
t1-t0
# 30 min for 100 on 16 cores  
# 4.8 hours for 1960 on 64 cores

