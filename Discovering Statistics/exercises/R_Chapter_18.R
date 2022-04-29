library(ggplot2)

x <- sample(x=1:100, size=100, replace = TRUE)

mean(x)

y <- x
y[sample(x=1:100, size=20, replace=FALSE)] <- NA
mean(y) # Will always return NA
mean(y, na.rm=TRUE) # Will ignore NAs

grades <- c(95, 72, 87, 66)
weights <- c(1/2, 1/4, 1/8, 1/8)
mean(grades)
weighted.mean(x=grades, w=weights)

var(x)
sum((x-mean(x))^2) / (length(x) - 1)

sqrt(var(x))
sd(x)
sd(y)
sd(y, na.rm=TRUE)

min(x)
max(x)
median(x)

min(y)
min(y, na.rm=TRUE)

summary(x)
summary(y)

quantile(x, probs=c(.25, .75))
quantile(y, probs=c(.25, .75))
quantile(y, probs=c(.25, .75), na.rm=TRUE)
quantile(x, probs=c(.1, .25, .5, .75, .99))

#------------------------------------------------------------

library(ggplot2)
head(economics)

cor(economics$pce, economics$psavert)

cor(economics[, c(2, 4:6)])

GGally::ggpairs(economics[, c(2, 4:6)])

library(reshape2)
library(scales)

econCor <- cor(economics[, c(2, 4:6)])
econMelt <- melt(econCor, varnames = c("x", "y"), value.name = "Correlation")
econMelt <- econMelt[order(econMelt$Correlation), ]

ggplot(econMelt, aes(x=x, y=y)) + geom_tile(aes(fill=Correlation)) + 
  scale_fill_gradient2(low="red", mid="white", high="steelblue", 
  guide=guide_colorbar(ticks=FALSE, baheight=10), limits = c(-1, 1)) + 
  theme_minimal() + labs(x=NULL, y=NULL)

m <- c(9, 9, NA, 3, NA, 5, 8, 1, 10, 4)
n <- c(2,  NA, 1, 6, 6, 4, 1, 1, 6, 7)
p <- c(8, 4, 3, 9, 10, NA, 3, NA, 9, 9)
q <- c(10, 10, 7, 8, 4, 2, 8, 5, 5, 2)
r <- c(1, 9, 7, 6, 5, 6, 2, 7, 9, 10)

theMat <- cbind(m, n, p, q, r)

cor(theMat, use="everything")
cor(theMat, use="all.obs")
cor(theMat, use="complete.obs")
cor(theMat, use="na.or.complete")
cor(theMat[c(1, 4, 7, 9, 10),])

cor(theMat, use="pairwise.complete.obs")
cor(theMat[, c("m", "n")], use="complete.obs")
cor(theMat[, c("m", "p")], use="complete.obs")

data(tips, package="reshape2")
GGally::ggpairs(tips)

library(RXKCD)
getXKCD(which="552")

cov(economics$pce, economics$psavert)
cov(economics[, c(2, 4:6)])

#----------------------------------------------------------

head(tips)
unique(tips$sex)
unique(tips$day)

t.test(tips$tip, alternative = "two.sided", mu=2.50)

randT <- rt(30000, df=NROW(tips)-1)
tipTTest <- t.test(tips$tip, alternative = "two.sided", mu=2.50)
ggplot(data.frame(x=randT)) +
  geom_density(aes(x=x), fill="grey", color="grey") +
  geom_vline(xintercept = tipTTest$statistic) + 
  geom_vline(xintercept = mean(randT) + c(-2, 2)*sd(randT), linetype=2)

t.test(tips$tip, alternative = "greater", mu=2.50)

aggregate(tip ~ sex, data=tips, var)
shapiro.test(tips$tip)

shapiro.test(tips$tip[tips$sex == "Female"])
shapiro.test(tips$tip[tips$sex == "Male"])

ggplot(tips, aes(x=tip, fill=sex)) + geom_histogram(binwidth = .5, alpha=1/2)

ansari.test(tip ~ sex, tips)

t.test(tip ~ sex, data=tips, var.equal=TRUE)

library(plyr)
tipSummary <- ddply(tips, "sex", summarize,
                    tip.mean=mean(tip), tip.sd=sd(tip),
                    Lower=tip.mean - 2*tip.sd/sqrt(NROW(tip)),
                    Upper=tip.mean + 2*tip.sd/sqrt(NROW(tip)))

ggplot(tipSummary, aes(x=tip.mean, y=sex)) + geom_point() + 
  geom_errorbarh(aes(xmin=Lower, xmax=Upper), height=.2)

#----------------------------------------------------------------------

data(father.son, package='UsingR')
head(father.son)

t.test(father.son$fheight, father.son$sheight, paired=TRUE)

heightDiff <- father.son$fheight - father.son$sheight
ggplot(father.son, aes(x=fheight - sheight)) + 
  geom_density() +
  geom_vline(xintercept = mean(heightDiff)) + 
  geom_vline(xintercept = mean(heightDiff) + 
               2*c(-1, 1) * sd(heightDiff) / sqrt(nrow(father.son)), linetype=2)

#----------------------------------------------------------------------

data(tips, package="reshape2")
tipAnova <- aov(tip ~ day - 1, tips)
tipIntercept <- aov(tip ~ day, tips)

tipAnova$coefficients
tipIntercept$coefficients

summary(tipAnova)

tipsByDay <- ddply(tips, "day", plyr::summarize,
                   tip.mean=mean(tip), tip.sd=sd(tip),
                   Length=NROW(tip),
                   tfrac=qt(p=.90, df=Length-1),
                   Lower=tip.mean - tfrac*tip.sd/sqrt(Length),
                   Upper=tip.mean + tfrac*tip.sd/sqrt(Length))

ggplot(tipsByDay, aes(x=tip.mean, y=day)) + geom_point() + 
  geom_errorbarh(aes(xmin=Lower, xmax=Upper), height=.3)