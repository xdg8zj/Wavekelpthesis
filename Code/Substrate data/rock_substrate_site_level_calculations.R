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

  sand_tran <- dt1 %>% dplyr:: select(YEAR, SITE, TRANSECT, QUAD_SIDE, COMMON_NAME, PERCENT_COVER) %>% dplyr:: group_by(YEAR, SITE, TRANSECT, COMMON_NAME) %>% dplyr::summarise(PERCENT_COVER = mean(PERCENT_COVER, na.rm = TRUE)) %>% ungroup() %>% pivot_wider(names_from=COMMON_NAME, values_from = PERCENT_COVER) %>% dplyr:: mutate(SAND_TOTAL = `Sand`, SAND_SS = `Sand` + `Shallow Sand`) %>% dplyr::select(YEAR, SITE, TRANSECT, SAND_TOTAL, SAND_SS) %>% dplyr::mutate(SAND_TOTAL = SAND_TOTAL + 0.0001, SAND_SS = SAND_SS + 0.0001)
  

  

sand_site <- dt1 %>% dplyr:: select(YEAR, SITE, TRANSECT, QUAD_SIDE, COMMON_NAME, PERCENT_COVER) %>% dplyr:: group_by(YEAR, SITE, TRANSECT, COMMON_NAME) %>% dplyr::summarise(PERCENT_COVER = mean(PERCENT_COVER, na.rm = TRUE)) %>% ungroup() %>% dplyr:: group_by(YEAR, SITE, COMMON_NAME) %>% dplyr:: summarise(PERCENT_COVER = mean(PERCENT_COVER, na.rm = TRUE)) %>% ungroup() %>% pivot_wider(names_from=COMMON_NAME, values_from = PERCENT_COVER) %>% dplyr:: mutate(SAND_TOTAL = `Sand`, SAND_SS = `Sand`+`Shallow Sand`) %>% dplyr::select(YEAR, SITE, SAND_TOTAL, SAND_SS) %>% dplyr::mutate(SAND_TOTAL = SAND_TOTAL + 0.0001, SAND_SS = SAND_SS + 0.0001)


