library(readxl)
library(ggplot2)

setwd("C:\\Users\\sksmi\\PeytoAccess\\Personal\\Bellevue\\DSC520\\dsc520")
housing_data <- read_xlsx("data\\week-7-housing.xlsx")
colnames(housing_data)[1:2] <- c("sale_date", "sale_price")

#Sale Warning 10568 na's
#ctyname 6068 na's

ggplot(housing_data) + 
  geom_histogram(aes(x = sale_price, fill=factor(sale_year)))

housing_data <- housing_data[housing_data$sale_price<2e6,]
housing_data$sale_year <- format(housing_data$sale_date, format = "%Y")
housing_data$bath_total <- with(housing_data, 
                                bath_full_count + 
                                  bath_half_count * .5 + 
                                  bath_3qtr_count * .75)

sale_price_lm1 <- lm(sale_price ~ sq_ft_lot, 
                     data=housing_data)

sale_price_lm2 <- lm(sale_price ~ sq_ft_lot + ctyname + year_built +
                     sale_year, data=housing_data)

summary(sale_price_lm1)
summary(sale_price_lm2)