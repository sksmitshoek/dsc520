theList <- list(A=matrix(1:9, 3), B=1:5, C=matrix(1:4, 2), D=2)
lapply(theList, sum)

library(purrr)
theList %>% map(sum)

theList2 <- theList
theList2[[1]][2, 1] <- NA
theList2[[2]][3] <- NA

theList2 %>% map(sum)
theList2 %>% map(function(x) sum(x, na.rm=TRUE))

theList2 %>% map(sum, na.rm=TRUE)

theList %>% map_int(NROW)

theList %>% map_int(mean) # Will result in an error
theList %>% map_dbl(mean)

theList[2] %>% map_chr(class)

theList %>% map_chr(class) # Will result in an error
theList %>% map(class)

theList %>% map_lgl(function(x) NROW(x) < 3)

buildDF <- function(x)
{
  data.frame(A=1:x, B=x:1)
}

listOfLengths <- list(3, 4, 1, 5)

listOfLengths %>% map(buildDF)
listOfLengths %>% map_df(buildDF)

theList %>% map_if(is.matrix, function(x) x*2)
theList %>% map_if(is.matrix, ~ .x*2)

data(diamonds, package='ggplot2')
diamonds %>% map_dbl(mean)

library(dplyr)
diamonds %>% summarize_each(funs(mean))

firstList <- list(A=matrix(1:16, 4), B=matrix(1:16, 2), C=1:5)
secondList <- list(A=matrix(1:16, 4), B=matrix(1:16, 8), C=15:1)

simpleFunc <- function(x, y)
{
  NROW(x) + NROW(y)
}

map2(firstList, secondList, simpleFunc)
map2_int(firstList, secondList, simpleFunc)

pmap(list(firstList, secondList), simpleFunc)
pmap_int(list(firstList, secondList), simpleFunc)