# Adrian Osterried, Arndt Glatz

my_matrix <- function(vec, nrow=NULL, ncol=NULL, colnames=NULL, rownames=NULL){
        
#### Part 1: Generating Matrix ######
        mat <- vec
        stopifnot('at least one of nrow, ncol has to be specified'= !(is.null(nrow) & is.null(ncol)))
        
        if(!is.null(nrow) & is.null(ncol)){
                stopifnot('incompatible length'= length(vec)%%nrow==0) 
                dim(mat) <- c(nrow, length(vec)/nrow)

        } else if(is.null(nrow) & !is.null(ncol)){
                stopifnot('incompatible length'= length(vec)%%ncol==0)
                dim(mat) <- c(length(vec)/ncol, ncol)
        
        } else if(!is.null(nrow) & !is.null(ncol) & length(vec)==1 ){
                mat <- rep(vec,nrow*ncol)
                dim(mat) <- c(nrow, ncol)
                
        } else if(!is.null(nrow) & !is.null(ncol)){
                stopifnot('incompatible length'= length(vec)==nrow*ncol)
                dim(mat) <- c(nrow, ncol)
        }
        
#### Part 2: Add rownames ######
        
        if(!is.null(rownames) & is.null(colnames)){
                stopifnot('length of rownames must be nrow' = length(rownames)==nrow)
                dimnames(mat) <- list(rows=rownames,NULL)
                
                
        } else if(is.null(rownames) & !is.null(colnames)){
                stopifnot('length of colnames must be ncol' = length(colnames)==ncol)
                dimnames(mat) <- list(NULL,columns=colnames)
                
                
        } else if(!is.null(rownames) & !is.null(colnames)){
                stopifnot('length of colnames must be ncol, length of rownames must be nrow' = 
                                  (length(colnames)==ncol) & (length(colnames)==ncol))
                dimnames(mat) <- list(rows=rownames, columns=colnames)
                
        }
                
        mat
}


my_matrix(1:6)

my_matrix(1:6, ncol=1)

my_matrix(1:6, ncol=2)

my_matrix(1:6, ncol=3)

my_matrix(1:6, ncol=6)

my_matrix(1:6, ncol=4)

my_matrix(1:6, nrow=2)

my_matrix(1:6, nrow=7)

my_matrix(1:6, ncol=2, nrow=2)

my_matrix(1:6, ncol=2, nrow=3)

my_matrix(1:6, ncol=2, nrow=1)

my_matrix(0, ncol=3, nrow=2)

my_matrix(1:6, ncol=3, colnames=LETTERS[1:3])

my_matrix(1:6, ncol=3, colnames=LETTERS[1:2])

my_matrix(1:6, ncol=3, rownames=letters[24 + 1:2])

my_matrix(1:6, ncol=3, colnames=LETTERS[1:3], rownames=letters[24 + 1:2])
