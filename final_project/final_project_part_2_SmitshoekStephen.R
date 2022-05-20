library(dplyr)

setwd("C:\\Users\\sksmi\\PeytoAccess\\Personal\\Bellevue\\DSC520\\dsc520")

global_temp <- read.csv("final_project\\data\\GlobalTemperatures.csv")
country_temp <- read.csv("final_project\\data\\GlobalLandTemperaturesByCountry.csv")
co2_atmo <- read.csv("final_project\\data\\co2_atmo.csv")
co2_emiss_country <- read.csv("final_project\\data\\co2_emission.csv")
sea_level <- read.csv("final_project\\data\\sea_levels_2015.csv")

global_temp$dt <- as.Date(global_temp$dt, "%Y-%m-%d")
global_temp[is.na(global_temp$LandAverageTemperature)==1,]$dt
global_temp <- global_temp[global_temp$dt >= as.Date("1753-01-01", "%Y-%m-%d"),]
global_temp <- subset(global_temp, select = c(dt, 
                                              LandAverageTemperature, 
                                              LandAverageTemperatureUncertainty))
global_temp <- aggregate(global_temp[,c(2,3)], by=list(format(global_temp$dt, format="%Y")), FUN=mean)
colnames(global_temp)[1] <- "year"

country_temp$dt <- as.Date(country_temp$dt, "%Y-%m-%d")
country_temp <- aggregate(cbind(AverageTemperature, AverageTemperatureUncertainty) ~ 
                            format(country_temp$dt, format="%Y") + Country, 
                          data = country_temp,
                          FUN=mean)
colnames(country_temp)[1] <- "year"


co2_atmo$date <- as.Date(paste0(co2_atmo$Year, "-", co2_atmo$Month, "-", "01"), 
                         "%Y-%m-%d")
co2_atmo <- subset(co2_atmo, select = c(date, 
                                        Carbon.Dioxide..ppm.,
                                        Seasonally.Adjusted.CO2..ppm.))
co2_atmo <- aggregate(co2_atmo[,c(2,3)], by=list(format(co2_atmo$date, format="%Y")), FUN=mean, na.action=na.omit)
colnames(co2_atmo) <- c("year","CO2_ppm", "CO2_ppm_Seas_Adj")

colnames(co2_emiss_country)[4] <- "co2_tonnes"
co2_emiss_global <- aggregate(co2_emiss_country$co2_tonnes, 
                              by=list(co2_emiss_country$Year), 
                              FUN=sum)
co2_emiss_global$year <- as.character(co2_emiss_global$year)
colnames(co2_emiss_global) <- c("year", "co2_tonnes")

sea_level$date <- as.Date(paste0(substring(sea_level$Time, 1, 4), "-", 
                                 "01", "-", "01"), "%Y-%m-%d")
sea_level <- aggregate(sea_level[,c(2,3)], by=list(format(sea_level$date, format="%Y")), FUN=mean)
colnames(sea_level)[1] <- "year"


climate_change_df <- left_join(global_temp, co2_atmo, by="year")
climate_change_df <- left_join(climate_change_df, co2_emiss_global, by="year")
climate_change_df <- left_join(climate_change_df, sea_level, by="year")

head(climate_change_df)