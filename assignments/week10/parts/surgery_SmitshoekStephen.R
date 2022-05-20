library(foreign)
library(caTools)

setwd("C:\\Users\\sksmi\\PeytoAccess\\Personal\\Bellevue\\DSC520\\dsc520")
surgery_data <- read.arff('data\\ThoraricSurgery.arff')

surgery_data_split <- sample.split(surgery_data, SplitRatio = 0.7)

surgery_training <- subset(surgery_data, surgery_data_split==TRUE)
surgery_testing <- subset(surgery_data, surgery_data_split==FALSE)

surgery_glm <- glm(Risk1Yr ~ DGN + PRE4 + PRE5 + PRE7 + PRE8 + PRE9 + PRE10 + 
                  PRE11 + PRE14 + PRE17 + PRE19 + PRE25 + PRE30 + PRE32 + AGE, 
                  data=surgery_training, family=binomial())


summary(surgery_glm)

response <- predict(surgery_glm, surgery_testing, type="response")

confmatrix <- table(Actual_Value=surgery_testing$Risk1Yr, Predicted_Value = response > 0.5)
confmatrix

(confmatrix[[1,1]] + confmatrix[[2,2]]) / sum(confmatrix)

