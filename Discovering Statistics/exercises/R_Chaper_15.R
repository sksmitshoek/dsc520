library(dplyr)
library(tibble)

sportLeague <- tibble(sport=c("Hockey", "Baseball", "Football"),
                      league=c("NHL", "MLB", "NFL"))
trophy <- tibble(trophy=c("Stanley Cup", "Commissioner's Trophy", "Vince Lombardi Trophy"))

trophies1 <- bind_cols(sportLeague, trophy)

trophies2 <- tribble(
  ~sport, ~league, ~trophy,
  "Basketball", "NBA", "Larry Trophy",
  "Golf", "PGA", "Wanamaker Trophy"
)

trophies <- bind_rows(trophies1, trophies2)

#------------------------------------------------

library(readr)
colorsURL <- "http://www.jaredlander.com/data/DiamondColors.csv"
diamondColors <- read_csv(colorsURL)

library(ggplot2)
data(diamonds, package="ggplot2")
unique(diamonds$color)

library(dplyr)
left_join(diamonds, diamondColors, by=c('color'='Color'))

left_join(diamonds, diamondColors, by=c('color'='Color')) %>%
  distinct(color, Description)

diamondColors %>% distinct(Color, Description)

semi_join(diamondColors, diamonds, by=c('Color'='color'))

anti_join(diamondColors, diamonds, by=c('Color'='color'))

diamondColors %>% filter(Color %in% unique(diamonds$color))
diamondColors %>% filter(!Color %in% unique(diamonds$color))

#-------------------------------------------------------------

library(readr)
emotion <- read_tsv('http://www.jaredlander.com/data/reaction.txt')

library(tidyr)
emotion %>% gather(key=Type, value=Measurment, Age, BMI, React, Regulate)

emotionLong <- emotion %>% 
  gather(key=Type, value=Measurment, Age, BMI, React, Regulate) %>%
  arrange(ID)

head(emotionLong, 20)

emotion %>% gather(key=Type, value=Measurment, -ID, -Test, -Gender)

emotionLong %>% spread(key=Type, value=Measurment)