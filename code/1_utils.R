# function that returns a list of values between start and end mile points
# 'start' is the start value
# 'end' is the end value
# 'int' defines the interval of breaks in the list
return_range <- function(start, end, int) {
  x <- start - start %% int + int
  y <- end - end %% int
  if (int <= y - x || y == x) {
    z <- seq(x, y, by = int)
  } else {
    z <- NULL
  }
  return(list(c(start, z)))
}

# function that returns the value of crash modificaiton factor (CMF)
# for the variable of interest
# 'beta' is the estimated coefficient for the variable
# 'test' is the value of the variable that you want CMF for
# 'base' is the reference value of variable for CMF calcualation 
return_cmf <- function(beta, test, base) exp(beta * (test - base))