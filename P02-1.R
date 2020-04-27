# Adrian Osterried, Arndt Glatz

lsq <- function(X, y) { 
        
        stopifnot('X or y have length 0' = length(X) & length(y)!=0 )
        
        stopifnot('X must be numeric' = is.numeric(X))
        stopifnot('X must be matrix' = is.matrix(X))
        
        stopifnot('y must be numeric' = is.numeric(y))
        stopifnot('y must be matrix or vector' = (is.matrix(y) | is.vector(y)))
        stopifnot('y is not allowed to have more than 1 column' = NCOL(y)==1)
        
        stopifnot('dimensions of X and y do not fit' = NCOL(t(X))==NROW(y))
        
        stopifnot('X may not contain NA' = sum(is.na(X))==0)
        stopifnot('y may not contain NA' = sum(is.na(y))==0)
        
        stopifnot('det(t(x) %*% X) must not be 0' = det(t(X) %*% X)!=0)
        
        A <- t(X) %*% X
        solve(A, t(X) %*% y) 
}




lsq(matrix(1:6, nrow=3), 1:3)

lsq(matrix(runif(6), nrow=3), matrix(runif(3), ncol=1))

lsq(matrix(letters[1:6] , nrow=2), 1:3)

lsq(matrix(1:6, nrow=3), list(1,2,3))

lsq(1:6, 1:3)

lsq(matrix(1:6, nrow=3), array(1:3, dim=c(1,1,3)))

lsq(matrix(1:6, nrow=3), 1:4)

lsq(matrix(1:6, nrow=3), matrix(1:3, nrow=1))

lsq(matrix(1:6, nrow=3), matrix(1:6, nrow=3))

lsq(matrix(double(0), nrow=0, ncol=0), matrix(double(0), nrow=0, ncol=0))

lsq(matrix(1:6, nrow=3), c(1,NA,3))

lsq(matrix(c(1:5, NA), nrow=3), 1:3)

lsq(matrix(c(1,1,2,1,1,2), nrow=3), 1:3)


      