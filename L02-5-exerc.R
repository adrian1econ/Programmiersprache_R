# Welche der beiden Sequenzen entstammt einem fairen Münzwurf (mit unabhängigen Würfe)?
x <- c(1,1,1,0,1,0,0,1,1,1,0,1,0,0,1,1,0,1,1,1,
       0,0,0,0,0,0,1,1,1,1,1,1,0,0,1,0,0,1,0,0,
       1,1,1,1,1,0,1,0,1,1,1,0,1,1,1,0,1,1,0,0,
       1,0,1,0,0,1,1,0,1,1,0,1,0,0,0,0,0,0,0,0,
       1,0,1,0,1,0,0,0,1,0,1,0,1,1,1,1,0,0,1,1)
y <- c(1,0,1,0,1,1,0,1,0,0,1,1,0,0,0,1,0,1,1,1,
       0,0,1,1,0,0,1,0,1,1,0,1,0,1,0,1,1,0,1,0,
       0,1,0,1,0,0,1,0,0,1,1,1,0,1,0,1,0,1,0,1,
       0,1,0,1,1,0,1,0,1,0,0,1,0,0,0,1,0,0,1,1,
       0,1,0,0,0,1,1,0,0,1,0,1,0,0,1,0,1,0,1,1)

c(length(x), length(y)) # gleiche Länge
table(x) # zähle Anzahl der 1en und 0en
table(y)





get_runs_statistic <- function(v) {
  
  number_of_ones <- sum(v==1)
  my_rle <- rle(v)
  longest_run <- max(my_rle[['lengths']])
  number_of_runs <- length(my_rle[['values']])
  c(number_of_runs = number_of_runs , longest_run = longest_run, number_of_ones = number_of_ones)
}



get_confidence_region <- function(v, alpha) {
  stopifnot(length(alpha)==1)
  q <- 1 - alpha
  stopifnot(q > 0 && q <= 1)
  i <- 0
  m <- mean(v)
  while(
    sum(m-i <= v & v <= m+i) / length(v) < q
  ) i <- i+0.5
  return(list(left=ceiling(m-i)-0.5, right=floor(m+i)+0.5))
}

n <- length(x)

rx <- get_runs_statistic(x)
ry <- get_runs_statistic(y)


# simulate fair coin tosses

library(tibble)
set.seed(0)

simu <- replicate(1e5, # execute following function call 1e5 times
  get_runs_statistic(sample(0:1, n, replace=TRUE)))
data <- as_tibble(t(simu))


# Plot results

layout(
  rbind(
    c(1, 2),
    c(3, 3) 
  ),
  heights=c(1, 1),
)
clrs <- list(x = "red", y = "blue", conf = "#00FF0040")
mark_lwd <- 3
invisible(lapply(names(data), function (nm) {
  v <- data[[nm]]
  hist(v, breaks=(min(v)-0.5):(max(v)+0.5), main=nm, xlab=NULL)
  y_axis_max <- par("yaxp")[2]
  segments(rx[nm], 0, y1=y_axis_max, col=clrs$x, lwd=mark_lwd)
  segments(ry[nm], 0, y1=y_axis_max, col=clrs$y, lwd=mark_lwd)
  conf <- get_confidence_region(v, 0.05)
  rect(conf$left, 0, conf$right, y_axis_max, col=clrs$conf, border=NA)
}))
legend(
  "topright", 
  c("x", "y", "95% confidence"), 
  col=c(clrs$x, clrs$y, clrs$conf), 
  lwd=mark_lwd)