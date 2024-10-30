rm(list = ls())
library(dplyr)
library(lubridate)
library(tidyr)
library(glue)

setwd("/Users/aleynaloughran-pierce/Desktop")
substrate <- read.csv("./Wavekelpthesis/Code/Substrate data/Annual_Substrate_All_Years_20240823.csv")


#Ethan's code 
# calculate rock cover for each transect
# the remaining cover is sand, such that: 100 - rock = sand
rock_tran <- substrate %>% 
  group_by(SITE, DATE, TRANSECT) %>% 
  filter(SUBSTRATE_TYPE == 'B' | SUBSTRATE_TYPE == 'BL' | SUBSTRATE_TYPE == 'BM' | SUBSTRATE_TYPE == 'BS' | SUBSTRATE_TYPE == 'C'| SUBSTRATE_TYPE == 'SS' | SUBSTRATE_TYPE == 'SH') %>% 
  summarize(rock = sum(PERCENT_COVER)/4) %>%
  mutate(YEAR = year(DATE)) %>%
  mutate(y = as.character(YEAR), s = as.character(SITE), t = as.character(TRANSECT)) %>%
   unite('SITE_YEAR', s:y) %>%
  unite('SITE_YEAR_TRAN', SITE_YEAR:t)
rock_site_transavg <- rock_tran[,c("SITE","YEAR","rock")]


rock_site <- substrate %>% 
  group_by(SITE, DATE) %>% 
  filter(SUBSTRATE_TYPE == 'B' | SUBSTRATE_TYPE == 'BL' | SUBSTRATE_TYPE == 'BM' | SUBSTRATE_TYPE == 'BS' | SUBSTRATE_TYPE == 'C'| SUBSTRATE_TYPE == 'SS' | SUBSTRATE_TYPE == 'SH') 

rock_site_avgs <- matrix(NA, dim(rock_site)[1], dim(rock_site)[2]-7) #make blank matrix with same number of rows but less columns (minus 2)
colnames(rock_site_avgs) <- colnames(rock_site)[c(1,4,10)] #assign column names to new matrix minus columns 3, 4, 5, and 7

Year_vector <- unique(rock_site$YEAR) #make unique vector based off of year column
Site_vector <- unique(rock_site$SITE) #make unique vector based off of site column
#make unique vector based off of coarse grouping

row_new <- 1 #number of rows added

for(i in Site_vector) #for element in site vector
{
  rock_site_stuff <- rock_site[rock_site$SITE == i,] #new matrix for everything lining up with site column
  for(j in Year_vector) #for all years
  {
    
    rock_site_thing <- rock_site_stuff[rock_site_stuff$YEAR == j,] #make matrix assigning everything by year
    
    mtrans <- length(unique(rock_site_thing$TRANSECT)) #number of transects = unique number of transect entries per year
    rock_site_avgs[row_new,1] <- j #column 1 assigns to the year with new data entry k
    rock_site_avgs[row_new,2] <- i
    
    out <- (sum(rock_site_thing$PERCENT_COVER))/mtrans #averages things in percent_cover column by number of transects and gets rid of nas--sums everything in quads and divides by number of transects
    
    if(is.nan(out)==TRUE) #if blank
    {
      next #assign na
    }
    
    rock_site_avgs[row_new,3] <- out #enter everything into the third column
      
    row_new <- row_new+1 #iterate by adding additional row
    
  }
}

rock_site_avgs <- as.data.frame(rock_site_avgs)
rock_site_avgs_1 <- rock_site_avgs %>% distinct()
rock_site_avgs_1 <- rock_site_avgs_1 %>% filter(!is.na(PERCENT_COVER))
rock_site_avgs_1 <- rock_site_avgs_1 %>% filter(SITE != "SCDI") 
rock_site_avgs_1 <- rock_site_avgs_1 %>% filter(SITE != "SCTW") 
colnames(rock_site_avgs_1)[3] <- "PERCENT_ROCK_COVER"


  # summarize(rock = sum(PERCENT_COVER)/length(unique(TRANSECT))) 
  #  mutate(YEAR = year(DATE)) %>%
  # mutate(y = as.character(YEAR), s = as.character(SITE))



rock_site_transquadavg <-substrate %>% 
  group_by(SITE, DATE) %>% 
  filter(SUBSTRATE_TYPE == 'B' | SUBSTRATE_TYPE == 'BL' | SUBSTRATE_TYPE == 'BM' | SUBSTRATE_TYPE == 'BS' | SUBSTRATE_TYPE == 'C'| SUBSTRATE_TYPE == 'SS' | SUBSTRATE_TYPE == 'SH') %>%
  summarize(rock = sum(PERCENT_COVER/4)/length(unique(TRANSECT))) %>%
  mutate(YEAR = year(DATE)) %>%
  mutate(y = as.character(YEAR), s = as.character(SITE))
rock_site_transquadavg <- rock_site_transquadavg[,c("SITE","YEAR","rock")]
  # unite('SITE_YEAR', s:y)
  # unite('SITE_YEAR_TRAN', SITE_YEAR:t)


rock_tran_avg 