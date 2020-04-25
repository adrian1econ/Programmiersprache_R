# Adrian Osterried
# Arndt Glatz


## Aufgabe 2
rm(list = ls())

eucl_dist <- function(x, y){
        
        if(length(x)==length(y)){
                res1 <- 'ok'
                res2 <- sqrt(sum((x-y)^2))
        } else {
                res1 <- sprintf('lengths differ: %d vs %d',length(x),length(y))
                res2 <- NaN
        }
        
        list(res1,res2)
}




x <- c(1,2)
y <- c(4, -2)
eucl_dist(x,y)

eucl_dist(1:4,1:5)


