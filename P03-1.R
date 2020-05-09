# Adrian Osterried, Arndt Glatz

`%o%` <- function(f,g) function(...) f(g(...))

mean_c <- mean %o% c 
mean_c(1,2,3,4)
