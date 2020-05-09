# Adrian Osterried, Arndt Glatz

(x <- matrix(1:16, nrow=4))

n_diag <- function(mat, n){
        m <- nrow(mat)
        neben <- c()
        
        for(i in 1:m){
                for (j in 1:m) {
                        if(i==j-n)
                        neben <- c(neben,mat[i,j])
                }
        }
        neben
}

n_diag(x, -3)

n_diag(x, -2)

n_diag(x, -1)

n_diag(x, 0)

n_diag(x, 1)

n_diag(x, 2)

n_diag(x, 3)

`n_diag<-` <- function(mat,n,value){
        m <- nrow(mat)
        vec <- logical(m*m)
        position <- 1
        
        for(i in 1:m){
                for (j in 1:m) {
                        if(i==j+n){
                                vec[position] <- T
                                position <- position+1
                        } else {
                                vec[position] <- F
                                position <- position+1
                        }
                }
        }
        mat[vec] <- value
        mat
}

(x <- matrix(1:9, nrow=3))

n_diag(x, -2) <- -1
x

n_diag(x, -1) <- -2
x

n_diag(x, 0) <- -1:-3
x

n_diag(x, 1) <- -11:-12
x

n_diag(x, 2) <- -42
x


