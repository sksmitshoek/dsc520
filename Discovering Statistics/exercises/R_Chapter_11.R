# apply only works on matrixes
the.matrix <- matrix(1:9, nrow=3)
the.matrix

apply(the.matrix, 1, sum) # sum the rows
apply(the.matrix, 2, sum) # sum the columns

the.matrix[2, 1] <- NA

apply(the.matrix, 1, sum)
apply(the.matrix, 1, sum, na.rm=TRUE)

#----------------------------------------------

the_list <- list(a=matrix(1:9, 3), b=1:5, c=matrix(1:4, 2), d=2)
# lapply works on lists or vectors and returns a list
lapply(the_list, sum)

#sapply works on lists or vectors and returns a vector
sapply(the_list, sum)

the_names <- c("Jared", "Deb", "Paul")
lapply(the_names, nchar)
sapply(the_names, nchar)

#----------------------------------------------

first_list <- list(a=matrix(1:16, 4), b=matrix(1:16, 2), c=1:5)
second_list <- list(a=matrix(1:16, 4), b=matrix(1:16, 8), c=15:1)

mapply(identical, first_list, second_list)

simple_function <- function(x, y){
  NROW(x) + NROW(y)
}

mapply(simple_function, first_list, second_list)

#----------------------------------------------

data(diamonds)
head(diamonds)

aggregate(price ~ cut, diamonds, mean, na.rm=TRUE)

aggregate(price ~ cut + color, diamonds, mean)

aggregate(cbind(price, carat) ~ cut, diamonds, mean)

aggregate(cbind(price, carat) ~ cut + color, diamonds, mean)

#----------------------------------------------

library(plyr)
head(baseball)

baseball$sf[baseball$year < 1954] <- 0
any(is.na(baseball$sf))

any(is.na(baseball$hbp))
baseball$hbp[is.na(baseball$hbp)] <- 0

baseball <- baseball[baseball$ab >= 50,]

baseball$OBP <- with(baseball, (h + bb + hbp) / (ab + bb + hbp + sf))
tail(baseball)

obp <- function(data){
  c(OBP=with(data, sum(h + bb + hbp) / sum(ab + bb + hbp + sf)))
}

careerOBP <- ddply(baseball, "id", obp)
careerOBP <- careerOBP[order(careerOBP$OBP, decreasing=TRUE), ]
head(careerOBP, 10)

#----------------------------------------------

aggregate(price ~ cut, diamonds, each(mean, median))

#----------------------------------------------

library(data.table)

the_df <- data.frame(A=1:10,
                     B=letters[1:10],
                     C=letters[11:20],
                     D=rep(c("One", "Two", "Three"),length.out=10))

the_dt <- data.table(A=1:10,
                     B=letters[1:10],
                     C=letters[11:20],
                     D=rep(c("One", "Two", "Three"),length.out=10))

str(the_df)
str(the_dt)

diamonds_dt <- data.table(diamonds)
diamonds_dt

the_dt[1:2, ]
the_dt[the_dt$A >= 7]
the_dt[A >= 7]
the_dt[, list(A, C)]
the_dt[, B]
the_dt[, list(B)]
the_dt[, "B", with=FALSE]
the_dt[, c("A", "C"), with=FALSE]
the_cols <- c("A", "C")
the_dt[, the_cols, with=FALSE]

tables()

setkey(the_dt, D)

the_dt["One",]
the_dt[c("One", "Two"), ]

setkey(diamonds_dt, cut, color)
diamonds_dt[J("Ideal", "E"), ]
diamonds_dt[J("Ideal", c("E", "D")), ]

aggregate(price ~ cut, diamonds, mean)
diamonds_dt[, mean(price), by=cut]
diamonds_dt[, list(price=mean(price)), by=cut]
diamonds_dt[, list(price=mean(price)), by=list(cut,color)]
diamonds_dt[, list(price=mean(price), carat=mean(carat)), by=cut]
diamonds_dt[, list(price=mean(price), carat=mean(carat), carat_sum=sum(carat)), by=cut]
diamonds_dt[, list(price=mean(price), carat=mean(carat)), by=list(cut, color)]




