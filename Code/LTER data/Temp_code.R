# Package ID: knb-lter-sbc.161.1 Cataloging System:https://pasta.edirepository.org.
# Data set title: Daily sea surface temperature in Santa Barbara channel between 1982 and 2021.
# Data set creator:    - Santa Barbara Coastal LTER 
# Data set creator:  Li Kui -  
# Contact:    - Information Manager, Santa Barbara Coastal LTER   - sbclter@msi.ucsb.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-sbc/161/1/7e820badb88a805c0c47a0c3ccec9bb3" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "date",     
                 "site",     
                 "latitude",     
                 "longitude",     
                 "temp"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

# attempting to convert dt1$date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1date<-as.Date(dt1$date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt1[dt1$date != "",]) == length(tmp1date[!is.na(tmp1date)])){dt1$date <- tmp1date } else {print("Date conversion failed for dt1$date. Please inspect the data and do the date conversion yourself.")}                                                                    

if (class(dt1$site)!="factor") dt1$site<- as.factor(dt1$site)
if (class(dt1$latitude)=="factor") dt1$latitude <-as.numeric(levels(dt1$latitude))[as.integer(dt1$latitude) ]               
if (class(dt1$latitude)=="character") dt1$latitude <-as.numeric(dt1$latitude)
if (class(dt1$longitude)=="factor") dt1$longitude <-as.numeric(levels(dt1$longitude))[as.integer(dt1$longitude) ]               
if (class(dt1$longitude)=="character") dt1$longitude <-as.numeric(dt1$longitude)
if (class(dt1$temp)=="factor") dt1$temp <-as.numeric(levels(dt1$temp))[as.integer(dt1$temp) ]               
if (class(dt1$temp)=="character") dt1$temp <-as.numeric(dt1$temp)

# Convert Missing Values to NA for non-dates

dt1$temp <- ifelse((trimws(as.character(dt1$temp))==trimws(".")),NA,dt1$temp)               
suppressWarnings(dt1$temp <- ifelse(!is.na(as.numeric(".")) & (trimws(as.character(dt1$temp))==as.character(as.numeric("."))),NA,dt1$temp))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(date)
summary(site)
summary(latitude)
summary(longitude)
summary(temp) 
# Get more details on character variables

summary(as.factor(dt1$site))
detach(dt1)               
