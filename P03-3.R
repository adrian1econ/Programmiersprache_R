# Adrian Osterried, Arndt Glatz

# 1. #########################################################

`size<-` <- function(mat,value){
        attr(mat,"size") <- value
        mat
}

size <- function(mat) attr(mat,"size")


# 2. #########################################################

sudoku <- function(mat, n, m){
        if(missing(m)){size(mat) <- c(n, ncol(mat)/n)
        } else if(missing(m)){size(mat) <- c(ncol(mat)/m, m)
        } else{size(mat) <- c(n,m)}
        
        len <- sqrt(length(mat))
        dim(mat) <- c(len,len)
        attr(mat,"class") <- "sudoku"
        
        mat
}


# 3. #########################################################

is_sudoku <- function(s){
        s_class <- attr(s,"class")
        s_size <- attr(s,"size")
        
        all(s_class=="sudoku", length(s_size==2))
}

# 4. #########################################################

is_sub_valid <- function(x){
        test1 <- c(NA,1:length(x))
        
        x_na <- x[!is.na(x)]
        x_unique <- unique(x_na)
                
        all(x %in% test1) && identical(x_na,x_unique)
}

# 5. #########################################################

partition_index <- function(n,m){
        
        if(m>=n){ 
                col1 <- rep(1:m,times=m,each=n)
                col2 <- col1+m
                vec <- c(col1,col2)
                return(matrix(vec,n*m))
        } else if(m<n){
                seq <- seq(1,n*m-1,by=m)
                seq1 <- rep(seq,each=m,times=n)
                seq2 <- seq1+1
                vec <- c(seq1,seq2)
                return(matrix(vec,n*m,byrow = T))}
        
}

# 6. #########################################################

is_valid <- function(s){
        row_valid <- apply(s,1,is_sub_valid)
        col_valid <- apply(s,2,is_sub_valid)
        
        n <- size(s)[1]
        m <- size(s)[2]
        
        field_valid <- sapply(1:(n*m), function(x) is_sub_valid(s[partition_index(n,m)==x]))
        
        all(row_valid, col_valid, field_valid)
}




is_filled_in <- function(s){
        filled <- c(1:prod(size(s)))
        all(s %in% filled)
}




is_solved <- function(s){
        is_filled_in(s) && is_valid(s)
}
        
# 7. #########################################################        





# 8. #########################################################   

print_non_valid <- function(s, print_missing=TRUE){
        
        n <- size(s)[1]
        m <- size(s)[2]
        len <- prod(size(s))
        
        matr <- outer(1:len, 1:len, FUN = "paste", sep = ",")
        NAs <- matr[is.na(s)]
        
        field <- outer(1:m,1:n,FUN = "paste", sep = ",")
        field_valid <- sapply(1:(n*m), function(x) is_sub_valid(s[partition_index(n,m)==x]))
        invalid_field <- field[!field_valid]
        
        rowcol <- as.character(1:len)
        row_valid <- apply(s,1,is_sub_valid)
        col_valid <- apply(s,2,is_sub_valid)
        invalid_row <- rowcol[!row_valid]
        invalid_col <- rowcol[!col_valid]
        
        if(print_missing==TRUE && !identical(NAs,character(0))){cat(paste('NA:', NAs),sep="\n")}
        if(!identical(invalid_field,character(0))){cat(paste('Invalid Field:', invalid_field),sep="\n")}
        if(!identical(invalid_row,character(0))){cat(paste('Invalid Row:', invalid_row),sep="\n")}
        if(!identical(invalid_col,character(0))){cat(paste('Invalid Col:', invalid_col),sep="\n")}
}
        
        
     



x <- c(5,2,6,4,3,1,6,1,3,2,5,4,3,4,1,5,2,6,2,6,4,3,1,5,1,3,5,6,4,2,4,5,2,1,6,3)
is_sudoku(x)

s <- sudoku(x, 3, 2)
s

size(s)

is_sudoku(s)

is_filled_in(s)

is_valid(s)

is_solved(s)

print_non_valid(s)

size(s) <- c(2, 3)
is_sudoku(s)

is_filled_in(s)

is_valid(s)

is_solved(s)


s_na <- s
s_na[sample(36, 18)] <- NA
s_na

is_sudoku(s_na)

is_filled_in(s_na)

is_valid(s_na)

is_solved(s_na)

print_non_valid(s_na)


s_not <- sudoku(sample(1:6, 36, replace=T), 2, 3)

s_not[sample(36, 18)] <- NA

s_not

is_sudoku(s_not)

is_filled_in(s_not)

is_valid(s_not)

is_solved(s_not)

print_non_valid(s_not, print_missing=FALSE)
partition_index(2,3)


