approx_p <- function(n0, a, b) {
  rand_numb <- rnorm(n0, 0, 1)
  rand_interval <- rand_numb[rand_numb>=a & rand_numb<=b]
  P <- length(rand_interval)/n0
}
# berechne wahres p durch Integration
p <- integrate(dnorm, lower=-1, upper=3)$value
p
# berechne Differenz zwischen N?herung und wahrem Wert f?r verschiedene n0
abs(p - approx_p(1e1, -1, 3))
abs(p - approx_p(1e3, -1, 3))
abs(p - approx_p(1e5, -1, 3))


