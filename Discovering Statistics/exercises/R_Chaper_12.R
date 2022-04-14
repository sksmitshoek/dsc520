library(magrittr)
data(diamonds, package='ggplot2')
dim(head(diamonds, n=4))

diamonds %>% head(4) %>% dim

class(diamonds)
head(diamonds)

library(dplyr)
diamonds

select(diamonds, carat, price)

diamonds %>% select(carat, price)
diamonds %>% select(c(carat, price))

diamonds %>% select(one_of('carat', 'price'))

the_cols <- c('carat', 'price')
diamonds %>% select(the_cols)

diamonds[, c('carat', 'price')]
select(diamonds, 1, 7)

diamonds %>% select(starts_with('c'))

diamonds %>% select(ends_with('e'))

diamonds %>% select(contains('l'))

diamonds %>% select(matches('r.+t'))

diamonds %>% select(-carat, -price)

diamonds %>% select(-c(carat, price))

diamonds %>% select(-'carat', -'price')

diamonds %>% filter(cut == 'Ideal')

diamonds %>% filter(cut %in% c('Ideal', 'Good'))

diamonds %>% filter(price >= 1000)

diamonds %>% filter(carat > 2, price < 14000)

diamonds %>% filter(carat>2 & price < 14000)

diamonds %>% filter(carat < 1 | carat > 5)

diamonds %>% filter_("cut == 'Ideal'")

diamonds %>% filter_(~cut == 'Ideal')

the_cut <- 'Ideal'
diamonds %>% filter_(~cut == the_cut)

the_cols <- 'cut'
diamonds %>% filter_(sprintf("%s == '%s'", the_cols, the_cut))

library(lazyeval)
interp(~a == b, a=as.name(the_cols), b=the_cut)

diamonds %>% filter_(interp(~a == b, a=as.name(the_cols), b=the_cut))


library(rlang)
diamonds %>% filter(UQE(as.name(the_cols)) == the_cut)

diamonds %>% slice(1:5)

diamonds %>% slice(1:5, 8, 15:20)

diamonds %>% slice(-1)

diamonds %>% mutate(price/carat)

diamonds %>% select(carat, price) %>% mutate(price/carat)

diamonds %>% select(carat, price) %>% mutate(Ratio=price/carat)

diamonds %>% select(carat, price) %>% 
  mutate(Ratio=price/carat, Double=Ratio*2)

diamonds2 <- diamonds

diamonds2 %<>% select(carat, price) %>% 
  mutate(Ratio=price/carat, Double=Ratio*2)

diamonds2 <- diamonds2 %>% mutate(Quadruple=Double*2)

diamonds %>% summarize(AvgPrice=mean(price), MedianPrice=median(price),
                       AVgCarat=mean(carat))

diamonds %>% group_by(cut) %>% summarize(AvgPrice=mean(price))

diamonds %>% group_by(cut) %>% summarize(AvgPrice=mean(price), SumCarat=sum(carat))

diamonds %>% group_by(cut, color) %>% summarize(AvgPrice=mean(price), SumCarat=sum(carat))

diamonds %>% group_by(cut) %>% 
  summarize(AvgPrice=mean(price), SumCarat=sum(carat)) %>%
  arrange(AvgPrice)

diamonds %>% group_by(cut) %>% 
  summarize(AvgPrice=mean(price), SumCarat=sum(carat)) %>%
  arrange(desc(AvgPrice))

topN <- function(x, N=5)
{
  x %>% arrange(desc(price)) %>% head(N)
}

diamonds %>% group_by(cut) %>% do(topN(., N=3))

diamonds %>% group_by(cut) %>% do(Top=topN(.,3))

topByCut <- diamonds %>% group_by(cut) %>% do(Top=topN(., 3))
class(topByCut)
class(topByCut$Top)
class(topByCut$Top[[1]])

download.file("http://www.jaredlander.com/data/diamonds.db", destfile="data\\diamonds.db", mod='wb')
diaDBSource <- src_sqlite("data\\diamonds.db")

diaDBSource2 <- DBI::dbConnect(RSQLite::SQLite(), "data\\diamonds.db")

diaTab <- tbl(diaDBSource, "diamonds")

diaTab %>% group_by(cut) %>% dplyr::summarize(Price=mean(price))
diaTab %>% group_by(cut) %>% dplyr::summarize(Price=mean(price), Carat=mean(carat))