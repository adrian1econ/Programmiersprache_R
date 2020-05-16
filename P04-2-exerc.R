# Adrian Osterried, Arndt Glatz

# install.packages("babynames")
library(tidyverse)

## Create some data-----------------------------------------------------------

set.seed(1)

baseset <- list()
baseset$grade <- as.integer(c(5,6,7,8,9,10,11))
baseset$grade_boost <- c(1,3,5,7,8,9,10)
baseset$letter <- letters[1:4]
baseset$letter_boost <- sample(1:5, 4, replace=T)
babynames::babynames %>%
  group_by(sex, name) %>% 
  summarise(n = sum(n)) %>%
  arrange(desc(n)) %>%
  mutate(rank = min_rank(-n)) %>% 
  filter (rank <= 3000) ->
  ranked_names
baseset$name <- ranked_names$name
baseset$distance <- c(100,200,400,1000)
baseset$distance_boost <- c(14,12,10,8)

sample_observation <- function(n) {
  res <- list()
  res$name <- sample(baseset$name, n, replace=T)
  res$grade <- sample(baseset$grade, n, replace=T)
  res$letter <- sample(baseset$letter, n, replace=T)
  boost_base <- 
    baseset$grade_boost[match(res$grade,baseset$grade)] + 
    baseset$letter_boost[match(res$letter,baseset$letter)]
  res$time100 <- sample_time(100, baseset$distance_boost[1] + boost_base)
  res$time200 <- sample_time(200, baseset$distance_boost[2] + boost_base)
  res$time400 <- sample_time(400, baseset$distance_boost[3] + boost_base)
  res$time1000 <- sample_time(1000, baseset$distance_boost[4] + boost_base)
  as_tibble(res)
}

sample_time <- function(dist, boost) {
  (runif(length(boost))/2+2.5)/boost*dist*2
}

sports <- sample_observation(1000)

requirements <- tibble(
  level = 1:11,
  min100 = seq(43,23,len=11),
  min1000 = seq(500,300,len=11)
)


## Exercises -----------------------------------------------------------------

# a) 
# sort sports by 'name' (alphabetically)
sports %>% arrange(name)

# b) 
# sort sports by 'grade' (11, 10, ..., 5), 
# in case of ties by 'letter' (a, b, d, e),
# in case of ties by 'name' (A-Z)
sports %>% arrange(desc(grade),letter,name)

# c)
# count the numbers of students per class
sports %>% 
  count(grade, letter)

# d)
# what is the mean, max and min class size
sports %>% 
  count(grade, letter) %>% 
  summarise(min=min(n),
            max=max(n),
            mean=mean(n)
  )
  
  

# e)
# get all students with a non-unqiue name
sports %>% filter(name!=unique(name))

# f)
# get the top 10 sprinters (100m)
sports %>% filter(rank(time100)<=10)

# g)
# get the slowest 10 sprinters (100m)
sports %>% filter(desc(rank(time100))<=10)

# h)
# remove 100m, 200m, and 400m, and add velocity in km/h for 1000m
sports %>% select(name, grade, time1000) %>% 
  mutate(velocity=1/(time1000/3600))

# i)
# rename 'grade' to 'level'
sports %>% select(name, level=grade, everything())


# j)
# calculate average and min times for 200m in each grade
sports %>% 
  group_by(grade) %>% 
  summarise(average=mean(time200),
            min=min(time200))
  

# k)
# select all time-columns
sports %>% select(contains("time"))

# l)
# show the fastest sprinter (100m) in each class 
# sorted by class (ie by grade and letter)
sports %>% 
  group_by(grade,letter) %>%
  filter(time100==min(time100)) %>% 
  arrange(grade, letter)


