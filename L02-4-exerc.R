n <- 300

u <- runif(n,min = 0, max = 1)
x <- qnorm(u,0,1)
y <- rnorm(n,0,1)

par(mfrow=c(1,2), mar=c(2,2,1,1))
qqplot(u, y, main="uniform vs normal")
abline(0, 1, col="gray")
qqplot(x, y, main="normal (inversion) vs normal")
abline(0, 1, col="gray")



