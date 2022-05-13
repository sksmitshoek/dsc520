data(father.son, package='UsingR')
library(ggplot2)

head(father.son)

ggplot(father.son, aes(x=fheight, y=sheight)) + 
  geom_point() +
  geom_smooth(method="lm") +
  labs(x="Fathers", y="Sons")

heightsLM <- lm(sheight ~ fheight, data=father.son)
heightsLM

summary(heightsLM)

#-------------------------------------------------------------------------------

data(tips, package="reshape2")
head(tips)

tipsAnova <- aov(tip ~ day -1, data=tips) # -1 eliminates the intercept
tipsLM <- lm(tip ~ day -1, data=tips)

summary(tipsAnova)
summary(tipsLM)

library(dplyr)

tipsByDay <- tips %>%
  group_by(day) %>%
  dplyr::summarize(
    tip.mean=mean(tip), tip.sd=sd(tip),
    Length=NROW(tip),
    tfrac=qt(p=.90, df=Length-1),
    Lower=tip.mean - tfrac*tip.sd/sqrt(Length),
    Upper=tip.mean + tfrac*tip.sd/sqrt(Length)
  )

tipsInfo <- summary(tipsLM)
tipsCoef <- as.data.frame((tipsInfo$coefficients[, 1:2]))
tipsCoef <- within(tipsCoef, {
  Lower <- Estimate - qt(p=0.90, df=tipsInfo$df[2]) * `Std. Error`
  Upper <- Estimate + qt(p=0.90, df=tipsInfo$df[2]) * `Std. Error`
  day <- rownames(tipsCoef)
})

ggplot(tipsByDay, aes(x=tip.mean, y=day)) + geom_point() +
  geom_errorbarh(aes(xmin=Lower, xmax=Upper), height=.3) +
  ggtitle("Tips by day calculated manually")

ggplot(tipsCoef, aes(x=Estimate, y=day)) + geom_point() +
  geom_errorbarh(aes(xmin=Lower, xmax=Upper), height=.3) +
  ggtitle("Tips by day from regression model")

#-------------------------------------------------------------------------------

housing <- read.table("http://www.jaredlander.com/data/housing.csv",
                      sep=",",
                      header=TRUE,
                      stringsAsFactors=FALSE)

names(housing) <- c("Neighborhood", "Class", "Units", "YearBuilt",
                    "SqFt", "Income", "IncomePerSqFt", "Expense",
                    "ExpensePerSqFt", "NetIncome", "Value",
                    "ValuePerSqFt", "Boro")

head(housing)

library(ggplot2)
library(coefplot)
ggplot(housing, aes(x=ValuePerSqFt)) + geom_histogram(binwidth = 10) +
  labs(x="Value per Square Foot")

ggplot(housing, aes(x=ValuePerSqFt, fill=Boro)) + 
  geom_histogram(binwidth = 10) +
  labs(x="Value per Square Foot")
  
ggplot(housing, aes(x=ValuePerSqFt, fill=Boro)) + 
  geom_histogram(binwidth = 10) +
  labs(x="Value per Square Foot") +
  facet_wrap(~Boro)

ggplot(housing, aes(x=SqFt)) + geom_histogram()
ggplot(housing, aes(x=Units)) + geom_histogram()
ggplot(housing[housing$Units < 1000,], aes(x=SqFt)) + geom_histogram()
ggplot(housing[housing$Units < 1000,], aes(x=Units)) + geom_histogram()

ggplot(housing, aes(x=SqFt, y=ValuePerSqFt)) + geom_point()
ggplot(housing, aes(x=Units, y=ValuePerSqFt)) + geom_point()
ggplot(housing[housing$Units < 1000,], aes(x=SqFt, y=ValuePerSqFt)) + geom_point()
ggplot(housing[housing$Units < 1000,], aes(x=Units, y=ValuePerSqFt)) + geom_point()

sum(housing$Units >= 1000)
housing <- housing[housing$Units < 1000,]

ggplot(housing, aes(x=SqFt, y=ValuePerSqFt)) + geom_point()
ggplot(housing, aes(x=log(SqFt), y=ValuePerSqFt)) + geom_point()
ggplot(housing, aes(x=SqFt, y=log(ValuePerSqFt))) + geom_point()
ggplot(housing, aes(x=log(SqFt), y=log(ValuePerSqFt))) + geom_point()

ggplot(housing, aes(x=Units, y=ValuePerSqFt)) + geom_point()
ggplot(housing, aes(x=log(Units), y=ValuePerSqFt)) + geom_point()
ggplot(housing, aes(x=Units, y=log(ValuePerSqFt))) + geom_point()
ggplot(housing, aes(x=log(Units), y=log(ValuePerSqFt))) + geom_point()

house1 <- lm(ValuePerSqFt ~ Units + SqFt + Boro, data=housing)
summary(house1)

house1$coefficients
coef(house1)

house2 <- lm(ValuePerSqFt ~ Units * SqFt + Boro, data=housing)
house3 <- lm(ValuePerSqFt ~ Units : SqFt + Boro, data=housing)

coef(house2)
coef(house3)

house4 <- lm(ValuePerSqFt ~ SqFt *Units*Income, housing)
coef(house4)

house5 <- lm(ValuePerSqFt ~ Class*Boro, housing)
house5$coefficients

house1.b <- lm(ValuePerSqFt ~ scale(Units) + scale(SqFt) + Boro, data=housing)
coefplot(house1.b, sort='mag')

house6 <- lm(ValuePerSqFt ~ I(SqFt/Units) + Boro, housing)
coef(house6)

multiplot(house1, house2, house3)

housingNew <- read.table("http://www.jaredlander.com/data/housingNew.csv",
                         sep=",", header=TRUE, stringsAsFactors = FALSE)

housingPredict <- predict(house1, newdata = housingNew, se.fit=TRUE,
                          interval="prediction", level=.95)
head(housingPredict$fit)
head(housingPredict$se.fit)













