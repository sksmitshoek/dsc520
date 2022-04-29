rnorm(n=10)
rnorm(n=10, mean=100, sd=20)

randNorm10 <- rnorm(10)
randNorm10
dnorm(randNorm10)
dnorm(c(-1, 0, 1))

#-------------------------------------------------

randNorm <- rnorm(30000)
randDensity <- dnorm(randNorm)

library(ggplot2)

ggplot(data.frame(x=randNorm, y=randDensity)) + aes(x=x, y=y) + 
  geom_point() + labs(x="Random Normal Variables", y="Density")

#-------------------------------------------------

pnorm(randNorm10)
pnorm(c(-3, 0, 3))
pnorm(-1)

pnorm(1) - pnorm(0)

pnorm(1) - pnorm(-1)

#-------------------------------------------------

p = ggplot(data.frame(x=randNorm, y=randDensity)) + aes(x=x, y=y) +
  geom_line() + labs(x="x", y="Density")

neg1seq <- seq(from=min(randNorm), to=-1, by=.1)
lessThanNeg1 <- data.frame(x=neg1seq, y=dnorm(neg1seq))
head(lessThanNeg1)
lessThanNeg1 <- rbind(c(min(randNorm), 0), 
                      lessThanNeg1, 
                      c(max(lessThanNeg1$x), 0))

p + geom_polygon(data=lessThanNeg1, aes(x=x, y=y))

neg1pos1Seq <- seq(from=-1, to=1, by=.1)
neg1to1 <- data.frame(x=neg1pos1Seq, y=dnorm(neg1pos1Seq))
head(neg1to1)
neg1to1 <- rbind(c(min(neg1to1$x), 0),
                 neg1to1,
                 c(max(neg1to1$x), 0))

p + geom_polygon(data=neg1to1, aes(x=x, y=y))

#-------------------------------------------------

randProb <- pnorm(randNorm)
ggplot(data.frame(x=randNorm, y=randProb)) + aes(x=x, y=y) + 
  geom_point() + labs(x="Random Normal Variables", y="Probablity")

randNorm10
qnorm(pnorm(randNorm10))

#-------------------------------------------------
#-------------------------------------------------

rbinom(n=1, size=10, prob=.4)
rbinom(n=5, size=10, prob=.4)
rbinom(n=10, size=10, prob=.4)

rbinom(n=1, size=1, prob=.4)
rbinom(n=5, size=1, prob=.4)
rbinom(n=10, size=1, prob=.4)

library(ggplot2)
binomData <- data.frame(Successes=rbinom(n=10000, size=10, prob=.3))
ggplot(binomData, aes(x=Successes)) + geom_histogram(binwidth=1)

binom5 <- data.frame(Successes=rbinom(n=10000, size=5, prob=.3), Size=5)
binom10 <- data.frame(Successes=rbinom(n=10000, size=10, prob=.3), Size=10)
binom100 <- data.frame(Successes=rbinom(n=10000, size=100, prob=.3), Size=100)
binom1000 <- data.frame(Successes=rbinom(n=10000, size=1000, prob=.3), Size=1000)

binomAll <- rbind(binom5, binom10, binom100, binom1000)

ggplot(binomAll, aes(x=Successes))  + geom_histogram() + facet_wrap(~ Size, scales="free")

#-------------------------------------------------

dbinom(x=3, size=10, prob=.3)
pbinom(q=3, size=10, prob=.3)

dbinom(x=1:10, size=10, prob=.3)
pbinom(q=1:10, size=10, prob=.3)

qbinom(p=.3, size=10, prob=.3)
qbinom(p=c(.3, .35, .4, .5, .6), size=10, prob=.3)

#-------------------------------------------------
#-------------------------------------------------

pois1 <- rpois(n=10000, lambda=1)
pois2 <- rpois(n=10000, lambda=2)
pois5 <- rpois(n=10000, lambda=5)
pois10 <- rpois(n=10000, lambda=10)
pois20 <- rpois(n=10000, lambda=20)

pois <- data.frame(Lambda.1=pois1, 
                   Lambda.2=pois2, 
                   Lambda.5=pois5, 
                   Lambda.10=pois10, 
                   Lambda.20=pois20)

library(reshape2)

pois <- melt(data=pois, variable.name="Lambda", value.name="x")

library(stringr)

pois$Lambda <- as.factor(as.numeric(str_extract(string=pois$Lambda, pattern="\\d+")))

library(ggplot2)
ggplot(pois, aes(x=x)) + geom_histogram(binwidth = 1) + 
  facet_wrap(~ Lambda) + ggtitle("Probability Mass Function")

ggplot(pois, aes(x=x)) + 
  geom_density(aes(group=Lambda, color=Lambda, fill=Lambda), adjust=4, alpha=.5) + 
  scale_colour_discrete() + scale_fill_discrete() +
  ggtitle("Probability Mass Function")
