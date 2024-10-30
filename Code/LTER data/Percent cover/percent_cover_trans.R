rm(list = ls())
rm(list = ls())
library(dplyr)
library(lubridate)
library(tidyr)
library(glmmTMB)

setwd("/Users/aleynaloughran-pierce/Desktop")
LTER_data <- read.csv("./Wavekelpannualcoverfiles/Annual_Cover_All_Years_20240823.csv")
#summer months range from early july to late october (month = 7: 10)

#read in species names data to get coarse groupings

#sorts out columns into year, site, percent_cover, scientific name, and coarse grouping
percent_cover_data <- LTER_data[,]
percent_cover_data$COARSE_GROUPING <- paste(percent_cover_data$MOBILITY,percent_cover_data$GROUP)

percent_cover_data <- percent_cover_data[,c("YEAR", "SITE", "TRANSECT","QUAD","SIDE","PERCENT_COVER", "SCIENTIFIC_NAME", "COARSE_GROUPING")]


#if the percent cover is missing, replace it with na
for(animal in 1:length(percent_cover_data$PERCENT_COVER))
{
  if(percent_cover_data$PERCENT_COVER[animal] == -99999)
  {
    percent_cover_data$PERCENT_COVER[animal] = NA
  }
}

#boring clam --> endolithic sessile invert
for(animal in 1:length(percent_cover_data$SCIENTIFIC_NAME))
{
  if(percent_cover_data$SCIENTIFIC_NAME[animal] == "Parapholas californica" | percent_cover_data$SCIENTIFIC_NAME[animal] == "Chaceia ovoidea" )
  {
    percent_cover_data$COARSE_GROUPING[animal] <- "ENDOLITHIC SESSILE INVERT"
  }
}

for(animal in 1:length(percent_cover_data$SCIENTIFIC_NAME))
{
  if(percent_cover_data$SCIENTIFIC_NAME[animal] == "Macrocystis pyrifera")
  {
    percent_cover_data$COARSE_GROUPING[animal] <- "GIANT KELP"
  }
}

for(animal in 1:length(percent_cover_data$SCIENTIFIC_NAME))
{
  if(percent_cover_data$COARSE_GROUPING[animal] == "SESSILE INVERT")
  {
    percent_cover_data$COARSE_GROUPING[animal] <- "EPILITHIC SESSILE INVERT"
  }
}

for(animal in 1:length(percent_cover_data$COARSE_GROUPING))
{
  if(percent_cover_data$COARSE_GROUPING[animal] == "SESSILE ALGAE")
  {
    percent_cover_data$COARSE_GROUPING[animal] <- "UNDERSTORY ALGAE"
  }
}

percent_cover_trans <- matrix(NA, dim(percent_cover_data)[1], dim(percent_cover_data)[2]-3) #make blank matrix with same number of rows but less columns (minus 2)
colnames(percent_cover_trans) <- colnames(percent_cover_data)[c(-4,-5,-7)] #assign column names to new matrix minus columns 3, 4, 5, and 7

Year_vector <- unique(percent_cover_data$YEAR) #make unique vector based off of year column
Site_vector <- unique(percent_cover_data$SITE) #make unique vector based off of site column
COARSE_GROUPING_vector <- unique(percent_cover_data$COARSE_GROUPING)
transect_vector <- unique(percent_cover_data$TRANSECT)


row_new <- 1 #number of rows added
for(i in Site_vector) #for element in site vector
{
  pc_stuff <- percent_cover_data[LTER_data$SITE == i,] #new matrix for everything lining up with site column
  for(j in Year_vector) #for all years
  {
    
    pc_thing <- pc_stuff[pc_stuff$YEAR == j,] #make matrix assigning everything by year
    
    for(k in transect_vector)
    {
      pc_crap <- pc_thing[pc_thing$TRANSECT == k,]
      
      
      
      blank <- all(is.na(pc_crap$PERCENT_COVER)) #everything that is na in percent_Cover
      
      for(l in COARSE_GROUPING_vector) #for everything in the different functional groups
      {
        
        percent_cover_trans[row_new,1] <- j #column 1 assigns to the year with new data entry k
        percent_cover_trans[row_new,2] <- i #column 2 assigns new row to column entry i
        percent_cover_trans[row_new,3] <- k #column 3 assigns new row to column entry k
        percent_cover_trans[row_new,5] <- l
        
        pc_knacks <- pc_crap[pc_crap$COARSE_GROUPING == l,]  #assigns by coarse grouping
        
        out <- (sum(pc_knacks$PERCENT_COVER)/4, na.rm = TRUE)
        
        if(length(out)==0) #if blank
        {
          out <- NA #assign na
          if(blank==FALSE) #if not blank
          {
            out <- 0 #then give it 0
          }
        }
        
        
        percent_cover_trans[row_new,4] <- out #enter everything into the third column
        
        row_new <- row_new+1 #iterate by adding additional row
      }
    }
  }
}




