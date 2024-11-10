rm(list = ls())
library(dplyr)
library(lubridate)
library(tidyr)
library(glue)

# Package ID: knb-lter-sbc.138.5 Cataloging System:https://pasta.edirepository.org.
# Data set title: SBC LTER: Reef: Kelp Forest Community Dynamics: Cover of bottom substrate and sand depth.
# Data set creator:  Daniel C Reed -  
# Data set creator:  Robert J Miller -  
# Contact:    -  Santa Barbara Coastal LTER  - sbclter@msi.ucsb.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-sbc/138/5/82d1b4ba2b2c1b5438ae1279e19bf68b" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "YEAR",     
                 "MONTH",     
                 "DATE",     
                 "SITE",     
                 "TRANSECT",     
                 "QUAD",     
                 "SIDE",     
                 "SUBSTRATE_TYPE",     
                 "COMMON_NAME",     
                 "PERCENT_COVER"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

# attempting to convert dt1$DATE dateTime string to R date structure (date or POSIXct)                                
dt1$QUAD_SITE <- dt1$QUAD_SIDE <- paste(dt1$QUAD, dt1$SIDE)


tmpDateFormat<-"%Y-%m-%d"
tmp1DATE<-as.Date(dt1$DATE,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt1[dt1$DATE != "",]) == length(tmp1DATE[!is.na(tmp1DATE)])){dt1$DATE <- tmp1DATE } else {print("Date conversion failed for dt1$DATE. Please inspect the data and do the date conversion yourself.")}                                                                    

if (class(dt1$SITE)!="factor") dt1$SITE<- as.factor(dt1$SITE)
if (class(dt1$TRANSECT)!="factor") dt1$TRANSECT<- as.factor(dt1$TRANSECT)
if (class(dt1$QUAD)!="factor") dt1$QUAD<- as.factor(dt1$QUAD)
if (class(dt1$SIDE)!="factor") dt1$SIDE<- as.factor(dt1$SIDE)
if (class(dt1$SUBSTRATE_TYPE)!="factor") dt1$SUBSTRATE_TYPE<- as.factor(dt1$SUBSTRATE_TYPE)
if (class(dt1$COMMON_NAME)!="factor") dt1$COMMON_NAME<- as.factor(dt1$COMMON_NAME)
if (class(dt1$PERCENT_COVER)=="factor") dt1$PERCENT_COVER <-as.numeric(levels(dt1$PERCENT_COVER))[as.integer(dt1$PERCENT_COVER) ]               
if (class(dt1$PERCENT_COVER)=="character") dt1$PERCENT_COVER <-as.numeric(dt1$PERCENT_COVER)
if(class(dt1$QUAD_SIDE) == "character") dt1$QUAD_SIDE <- as.factor(dt1$QUAD_SIDE)

# Convert Missing Values to NA for non-dates

dt1$SITE <- as.factor(ifelse((trimws(as.character(dt1$SITE))==trimws("-99999")),NA,as.character(dt1$SITE)))
dt1$TRANSECT <- as.factor(ifelse((trimws(as.character(dt1$TRANSECT))==trimws("-99999")),NA,as.character(dt1$TRANSECT)))
dt1$QUAD <- as.factor(ifelse((trimws(as.character(dt1$QUAD))==trimws("-99999")),NA,as.character(dt1$QUAD)))
dt1$SIDE <- as.factor(ifelse((trimws(as.character(dt1$SIDE))==trimws("-99999")),NA,as.character(dt1$SIDE)))
dt1$SUBSTRATE_TYPE <- as.factor(ifelse((trimws(as.character(dt1$SUBSTRATE_TYPE))==trimws("-99999")),NA,as.character(dt1$SUBSTRATE_TYPE)))
dt1$COMMON_NAME <- as.factor(ifelse((trimws(as.character(dt1$COMMON_NAME))==trimws("-99999")),NA,as.character(dt1$COMMON_NAME)))
dt1$PERCENT_COVER <- ifelse((trimws(as.character(dt1$PERCENT_COVER))==trimws("-99999")),NA,dt1$PERCENT_COVER)               
suppressWarnings(dt1$PERCENT_COVER <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$PERCENT_COVER))==as.character(as.numeric("-99999"))),NA,dt1$PERCENT_COVER))
dt1$QUAD_SIDE <- as.factor(ifelse((trimws(as.character(dt1$QUAD_SIDE))==trimws("-99999 -99999")),NA,as.character(dt1$QUAD_SIDE)))

sand_tran <- dt1 %>% dplyr:: select(YEAR, SITE, TRANSECT, QUAD_SIDE, COMMON_NAME, PERCENT_COVER) %>% dplyr:: group_by(YEAR, SITE, TRANSECT, COMMON_NAME) %>% dplyr::summarise(PERCENT_COVER = mean(PERCENT_COVER, na.rm = TRUE)) %>% ungroup() %>% dplyr::mutate(PERCENT_COVER = PERCENT_COVER+0.0001) %>% pivot_wider(names_from=COMMON_NAME, values_from = PERCENT_COVER) %>% dplyr:: mutate(SAND_total = `Sand` + `Shallow Sand`) %>% dplyr::select(YEAR, SITE, TRANSECT, SAND_total)

sand_site <- dt1 %>% dplyr:: select(YEAR, SITE, TRANSECT, QUAD_SIDE, COMMON_NAME, PERCENT_COVER) %>% dplyr:: group_by(YEAR, SITE, TRANSECT, COMMON_NAME) %>% dplyr::summarise(PERCENT_COVER = mean(PERCENT_COVER, na.rm = TRUE)) %>% ungroup() %>% dplyr:: group_by(YEAR, SITE, COMMON_NAME) %>% dplyr:: summarise(PERCENT_COVER = mean(PERCENT_COVER, na.rm = TRUE)) %>% ungroup() %>% dplyr::mutate(PERCENT_COVER = PERCENT_COVER+0.0001) %>% pivot_wider(names_from=COMMON_NAME, values_from = PERCENT_COVER) %>% dplyr:: mutate(SAND_total = `Sand` + `Shallow Sand`) %>% dplyr::select(YEAR, SITE, SAND_total)


#Ethan's code 
# calculate rock cover for each transect
# the remaining cover is sand, such that: 100 - rock = sand
rock_tran <- substrate %>% 
  group_by(SITE, DATE, TRANSECT) %>% 
  filter(SUBSTRATE_TYPE == 'B' | SUBSTRATE_TYPE == 'BL' | SUBSTRATE_TYPE == 'BM' | SUBSTRATE_TYPE == 'BS' | SUBSTRATE_TYPE == 'C'| SUBSTRATE_TYPE == 'SS' | SUBSTRATE_TYPE == 'SH') 
#   summarize(rock = sum(PERCENT_COVER)/4) %>%
#   mutate(YEAR = year(DATE)) %>%
#   mutate(y = as.character(YEAR), s = as.character(SITE), t = as.character(TRANSECT)) %>%
#    unite('SITE_YEAR', s:y) %>%
#   unite('SITE_YEAR_TRAN', SITE_YEAR:t)
# rock_site_transavg <- rock_tran[,c("SITE","YEAR","rock")]


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