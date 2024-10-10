rm(list = ls())
library(dplyr)
library(lubridate)


#read in wave raw data
setwd("/Users/aleynaloughran-pierce/Desktop")
raw_cdip_data <- read.csv("./Wavekelpthesis/Raw Files/cdip_mop_hs_tp_daily_20240509.csv")

#Sets date row to datetime
raw_cdip_data$date_GMT <- as.Date(raw_cdip_data$date_GMT, format = "%Y-%m-%d")
#subsets it to site, date, mean wave, max waves
modified_wave_data <- raw_cdip_data[,c("site","date_GMT","Mean_Hs_m", "Max_Hs_m")]
#creates new row for winter year
modified_wave_data[,'wave_yr'] = NA

#converts month and year formats to numeric
month <- as.numeric(format(modified_wave_data$date_GMT, "%m"))
year <- as.numeric(format(modified_wave_data$date_GMT, "%Y"))



#goes through each row and looks at month. If month is jan, feb, or march, it's included in the previous year's winter. If it is in oct, nov, dec, it is in the current year's winter
for(m in 1:length(modified_wave_data$date_GMT)){
  if(month[m] == 01|month[m] == 02|month[m] == 03){
    modified_wave_data$wave_yr[m] <- year[m]
  }
  if(month[m] == 10|month[m] == 11|month[m] == 12){
    modified_wave_data$wave_yr[m] <- year[m]+1
   }
  else{
    next
  }
}

#filter out all the NA rows
modified_wave_data <- modified_wave_data %>% filter(!is.na(wave_yr))
# 
mod_wave_subset <- modified_wave_data %>% group_by(site,wave_yr) %>% summarize(Mean_Hs_m = mean(Mean_Hs_m), Max_Hs_m = max(Max_Hs_m), .groups = "keep")
# 
waveyr_maxhs <- plot(mod_wave_subset$wave_yr, mod_wave_subset$Max_Hs_m, type = "p", xlab = "Wave year", ylab = "Maxiumum wave height (m)", main = "Wave year vs max wave height (m)")
# 
write.csv(mod_wave_subset, "./Wavekelpthesis/Edited_data/mod_wave_subset.csv")

