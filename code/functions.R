# function that returns a list of values between start and end mile points
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
