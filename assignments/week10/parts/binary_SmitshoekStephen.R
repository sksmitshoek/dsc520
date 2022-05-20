library(caTools)

setwd("C:\\Users\\sksmi\\PeytoAccess\\Personal\\Bellevue\\DSC520\\dsc520")

binary_data <- read.csv('data\\binary-classifier-data.csv')
binary_data$label <- as.factor(binary_data$label)

binary_data_split <- sample.split(binary_data, SplitRatio = 0.7)

training_data <- subset(binary_data, binary_data_split==TRUE)
testing_data <- subset(binary_data, binary_data_split==FALSE)

binary_glm <- glm(label ~ x + y, data=training_data, family=binomial())

response <- predict(binary_glm, newdata=testing_data, type="response")

confmatrix <- table(Actual_Value=testing_data$label, Predicted_Value = response > 0.5)
confmatrix

accuracy <- (confmatrix[[1,1]] + confmatrix[[2,2]]) / sum(confmatrix)
accuracy