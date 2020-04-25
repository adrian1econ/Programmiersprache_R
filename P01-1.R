# Adrian Osterried
# Arndt Glatz


## Aufgabe 1 
rm(list = ls())

# 1.
x <- c(1.3,0.5,42,-8e-5)

# 2.
x[1:4] <- x[4:1]

# 3.
x[x<1] <- -2

# 4.
sum(x^2)

# 5.
a <- seq(1,99,2)
b <- rep(0,50)
q=c()

for (i in seq_along(b)) q <- c(q,a[i],b[i])

