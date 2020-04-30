# linear congruential generator (lcg)
lcg_prng <- function(n, y1, m, a, b) {
  
        seq <- y1
        count <- 1
        
        while(count<n){
                y_i <- (a* tail(seq,1) +b) %% m
                seq <- c(seq,y_i)
                count <- count+1
        
        }
        
        seq
}


lcg_prng(18, y1=1L, m=9L, a=4L, b=7L)

lcg_prng(18, y1=1L, m=9L, a=3L, b=3L)

lcg_prng(25, y1=1L, m=7, a=15, b=23)


par(mfrow=c(1,2))
m <- 2^11
x <- lcg_prng(500, y1=1, m=m, a=1017, b=1) 
plot(x[1:499]/m, x[2:500]/m, pch=".", cex=4, 
     xlab="u1, ..., u499", ylab="u2, ..., u500", main="lcg_prng()") 

# Vergleich mit R-Funktion sample.int(), siehe ?sample
x <- sample.int(m, 500)
plot(x[1:499]/m, x[2:500]/m, pch=".", cex=4,
     xlab="u1, ..., u499", ylab="u2, ..., u500", main="sample.int()")