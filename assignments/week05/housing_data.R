library(readxl)
library(dplyr)
library(purrr)
library(stringr)

housing_data <- read_xlsx("data\\week-7-housing.xlsx")
colnames(housing_data)[1:2] <- c("sale_date", "sale_price")
housing_data
head(housing_data)

#------------------------------------------------------------------

# a.

# summarize and group_by
housing_data %>% group_by(ctyname) %>% summarize(mean(sale_price))

# mutate and select
housing_data %>% 
  select(sale_price, square_feet_total_living) %>%
  mutate(sale_price/square_feet_total_living)

#filter
housing_data %>% filter(bath_full_count > 2, sale_price < 300000)

#arrange
housing_data %>% arrange(desc(sale_price))

#------------------------------------------------------------------

# b.

housing_data %>% map(median)
housing_data$sale_price %>% discard(function(x) x>300000)

#------------------------------------------------------------------

# c.

housing_data_subset_1 <- housing_data[1:10,]
housing_data_subset_2 <- housing_data[25:30,]

housing_data_subset_comb <- rbind(housing_data_subset_1, housing_data_subset_2)

housing_data_subset_1 <- housing_data[1:10,1:4]
housing_data_subset_2 <- housing_data[1:10,5:24]

housing_data_subset_comb <- cbind(housing_data_subset_1, housing_data_subset_2)

#------------------------------------------------------------------

# d.

some_string <- "This is a string of text.  We will split it apart, then concatonate it back together."

string_list <- strsplit(some_string, split = " ")

new_string <- paste(string_list[[1]], collapse=" ")