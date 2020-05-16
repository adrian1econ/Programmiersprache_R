# Adrian Osterried, Arndt Glatz

library(tidyverse)

lines <- read_lines("books.txt")

# Entfernen von Spaces und alles Uppercase
lines1 <- str_to_upper(lines)
lines1 <- str_replace_all(lines1, " ", "")
lines1 <- str_replace_all(lines1, ",", "")

# Entfernen der Klammern
lines2 <- str_replace_all(lines1, "\\(.*?\\)", "")


# Splitten der Strings
lines3 <- str_split(lines2,"[;.]")
lines3 <- unlist(lines3, use.names=FALSE)

# Extrahieren Buchstaben
category <- str_extract(lines3,"[:alpha:]{1,10}")
category <- unname(sapply(category, function(i)paste(sort(unlist(str_split(i, ""))), collapse="")))

count <- as.integer(str_extract(lines3,"[:digit:]{1,10}"))

# Tibble
data <- tibble(category, count)
data <- data %>% arrange(category)

data


cat_let <- "R"

books_of_category <- function(data, cat_let) {
        
        data1 <- data[str_which(data$category,fixed(cat_let)),]
        apply(data1, 1, function(x) cat("We have ", x[2], " books of category ", x[1],".", "\n", sep=""))
        invisible(NULL)
}


books_of_category(data, "R")
