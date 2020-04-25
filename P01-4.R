# Adrian Osterried
# Arndt Glatz


## Aufgabe 4
rm(list = ls())

least_squares_estimator <- function(x,y){
        beta <- solve(t(x) %*% x) %*% t(x) %*% y
}

# create data --------------------------------------------------------
beta <- c(1, 2)
n <- 20
x <- runif(n) 
X <- matrix(c(rep(1, n), x), ncol=2)
noise <- rnorm(n, mean=0, sd=0.5) # normally distributed noise
# sd = 0.5  =>  var = 0.5^2 = 0.25
y <- X %*% beta + noise # data from linear model with noise

# show created objects -----------------------------------------------
beta
str(x) # atomic vector length 20
str(X) # 20x2 matrix
str(noise) # atomic vector length 20
str(y) # 20x1 matrix (~ vector length 20)

# plot data ----------------------------------------------------------
plot(x, y)

beta_est <- least_squares_estimator(X,y)

plot(x,y)
abline(beta, col='blue')
abline(beta_est, col='red')
