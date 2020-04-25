# Adrian Osterried
# Arndt Glatz


## Aufgabe 3
rm(list = ls())

find_primes <- function(count){
        check_number <- 2
        i <- 0
        primes <- c()
        
        while(i<count){
        check_sequence <- seq(1,check_number,1)
        
        if (sum(check_number%%check_sequence == 0)==2){
                primes <- c(primes,check_number)
                i <- i+1
                check_number <- check_number+1
                
        } else check_number <- check_number+1 }
        
        primes
        
}


find_primes(5)
find_primes(1000)
