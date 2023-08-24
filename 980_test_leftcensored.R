
library(rjags)
library(runjags)

devtools::install_github("DagHjermann/leftcensored")

#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Package leftcensored ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

# Simulate data and estimate regression 
set.seed(11)
sim <- lc_simulate(n = 30)   # also plots the data

# Perform estimation
result <- lc_linear(sim$data)

# Get best estimate fitted line       
a <- result$intercept["50%"]
b <- result$slope["50%"]
# Add regression line to the plot  
abline(a, b, col = "green3")
# Add confidence interval  
lines(y_lo ~ x, data = result$plot_data, lty = "dashed", col = "green3")
lines(y_hi ~ x, data = result$plot_data, lty = "dashed", col = "green3")

# DIC
result$dic


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Parallel processing using doParallel - test ----
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o

install.packages("doParallel")

library(doParallel)
cl <- makeCluster(6)
registerDoParallel(cores = 6)

my_data <- iris[which(iris[,5] != "setosa"), c(1,5)]
trials <- 3

calculate_coefficients <- function(){
  ind <- sample(100, 100, replace=TRUE)
  result1 <- glm(my_data[ind,2] ~ my_data[ind,1], family=binomial(logit))
  coefficients(result1)
}
calculate_coefficients()

result <- foreach(icount(trials), 
                  .combine=cbind, 
                  .export = c("my_data","calculate_coefficients")) %dopar%
  calculate_coefficients()

result


#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o
#
# Parallel processing using doParallel ----
#
# Using thin-plate splines
#
#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o#o


# Simulate data and estimate regression 

# doesn't work? why not?
# simlist <- vector(length = 4, mode = "list") map(1:4, lc_simulate, n = 30, plot = FALSE)   # also plots the data
# simlist_data <- map(simlist, "data")

simlist <- list()
for (i in 1:10){
  set.seed(10 + i)
  simlist[[i]] <- lc_simulate(n = 30, plot = FALSE)   # also plots the data
}

lc_plot(simlist[[1]]$dat)

simlist_data <- map(simlist, "data")

get_coefficients <- function(data){
  # Perform estimation
  result <- lc_linear(data)
  # Get best estimate fitted line       
  c(a = result$intercept["50%"], b = result$slope["50%"])
}

get_coefficients(simlist_data[[1]])

#
# Non-parallell (for 5 series only)
#

t0 <- Sys.time()
test_result <- list()
for (i in 1:5){
  test_result[[i]] <- get_coefficients(simlist_data[[i]])
}
t1 <- Sys.time()
t1-t0
# 44.76803 secs

#
# Parallel (for 10 series)
#

t0 <- Sys.time()
result <- foreach(i = 1:10, 
                  .combine=rbind, 
                  .export = c("simlist_data","get_coefficients")) %dopar%
  get_coefficients(data = simlist_data[[i]])
t1 <- Sys.time()
t1-t0
# 52.43795 secs  


