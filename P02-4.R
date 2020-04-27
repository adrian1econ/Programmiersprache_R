# Adrian Osterried, Arndt Glatz

collatz <- function(x, max_iter){ 
        stopifnot('Cannot interpret x as integer' = !is.na(as.numeric(x)))
        stopifnot('Length of x must be 1' = length(x)==1)
        
        my_seq <- as.integer(x)
        count <- 1L
        
        while( count<max_iter && tail(my_seq,1)!=1 ){
                if(tail(my_seq,1)%%2 == 0){
                        my_seq <- as.integer(c(my_seq, tail(my_seq,1)%/%2))
        
                } else if(tail(my_seq,1)%%2 != 0) {
                        my_seq <- as.integer(c(my_seq, 3*tail(my_seq,1)+1))
                        
                }
                count <- count + 1L
        }
        
        if(tail(my_seq,1)!=1) count <- NA_integer_
        
        list(seq=my_seq, len=count)
}


str(collatz(1, 1e4))
#
str(collatz(2, 1e4))
#
str(collatz(3, 1e4))
#
str(collatz(3, 5))
#
str(collatz("4", 1e4))
#
str(collatz("four", 1e4))
#
str(collatz(1:5, 1e4))
#
str(collatz(5.0, 1e4))

str(collatz(5.1, 1e4))

str(collatz(5.9, 1e4))
