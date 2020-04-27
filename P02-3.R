# Adrian Osterried, Arndt Glatz

my_tibble <- function(data) { 
        attr(data, "class") <- c("tbl_df","tbl","data.frame")
        attr(data, 'row.names') <- 1:length(data[[1]])
        data
}




my_factor <- function(data) { 
        level <- unique(data)
        factors <- match(data,level)
        attr(factors, 'levels') <- level
        attr(factors, 'class') <- 'factor'
        
        factors
}


my_tb <- my_tibble(list(x=1:3, y=letters[1:3]))
my_tb

tb <- tibble(x=1:3, y=letters[1:3])
identical(tb, my_tb)


my_fac <- my_factor(c("a", "b", "a", "a", "c", "c"))
my_fac

fac <- factor(c("a", "b", "a", "a", "c", "c"))
identical(fac, my_fac)