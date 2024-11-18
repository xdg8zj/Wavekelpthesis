
# Package ID: knb-lter-sbc.50.17 Cataloging System:https://pasta.edirepository.org.
# Data set title: SBC LTER: Reef: Annual time series of biomass for kelp forest species, ongoing since 2000.
# Data set creator:  Daniel C Reed -  
# Data set creator:  Robert J Miller -  
# Contact:    -  Santa Barbara Coastal LTER  - sbclter@msi.ucsb.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

rm(list = ls())
rm(list = ls())
library(dplyr)
library(lubridate)
library(tidyr)
library(glue)
library(glmmTMB)
setwd("/Users/aleynaloughran-pierce/Desktop")


inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-sbc/50/17/24d18d9ebe4f6e8b94e222840096963c" 
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
                 "VIS",     
                 "SP_CODE",     
                 "PERCENT_COVER",     
                 "DENSITY",     
                 "WM_GM2",     
                 "DRY_GM2",     
                 "SFDM",     
                 "AFDM",     
                 "SCIENTIFIC_NAME",     
                 "COMMON_NAME",     
                 "TAXON_KINGDOM",     
                 "TAXON_PHYLUM",     
                 "TAXON_CLASS",     
                 "TAXON_ORDER",     
                 "TAXON_FAMILY",     
                 "TAXON_GENUS",     
                 "GROUP",     
                 "MOBILITY",     
                 "GROWTH_MORPH",     
                 "COARSE_GROUPING"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

# attempting to convert dt1$DATE dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1DATE<-as.Date(dt1$DATE,format=tmpDateFormat)


# Keep the new dates only if they all converted correctly
if(nrow(dt1[dt1$DATE != "",]) == length(tmp1DATE[!is.na(tmp1DATE)])){dt1$DATE <- tmp1DATE } else {print("Date conversion failed for dt1$DATE. Please inspect the data and do the date conversion yourself.")}      

for(animal in 1:length(dt1$SCIENTIFIC_NAME))
{
  if(dt1$SCIENTIFIC_NAME[animal] == "Parapholas californica" | dt1$SCIENTIFIC_NAME[animal] == "Chaceia ovoidea" )
  {
    dt1$COARSE_GROUPING[animal] <- "ENDOLITHIC SESSILE INVERT"
  }
}

for(animal in 1:length(dt1$COARSE_GROUPING))
{
  if(dt1$COARSE_GROUPING[animal] == "SESSILE INVERT")
  {
    dt1$COARSE_GROUPING[animal] <- "EPILITHIC SESSILE INVERT"
  }
}


if (class(dt1$SITE)!="factor") dt1$SITE<- as.factor(dt1$SITE)
if (class(dt1$TRANSECT)!="factor") dt1$TRANSECT<- as.factor(dt1$TRANSECT)
if (class(dt1$VIS)=="factor") dt1$VIS <-as.numeric(levels(dt1$VIS))[as.integer(dt1$VIS) ]               
if (class(dt1$VIS)=="character") dt1$VIS <-as.numeric(dt1$VIS)
if (class(dt1$SP_CODE)!="factor") dt1$SP_CODE<- as.factor(dt1$SP_CODE)
if (class(dt1$PERCENT_COVER)=="factor") dt1$PERCENT_COVER <-as.numeric(levels(dt1$PERCENT_COVER))[as.integer(dt1$PERCENT_COVER) ]               
if (class(dt1$PERCENT_COVER)=="character") dt1$PERCENT_COVER <-as.numeric(dt1$PERCENT_COVER)
if (class(dt1$DENSITY)=="factor") dt1$DENSITY <-as.numeric(levels(dt1$DENSITY))[as.integer(dt1$DENSITY) ]               
if (class(dt1$DENSITY)=="character") dt1$DENSITY <-as.numeric(dt1$DENSITY)
if (class(dt1$WM_GM2)=="factor") dt1$WM_GM2 <-as.numeric(levels(dt1$WM_GM2))[as.integer(dt1$WM_GM2) ]               
if (class(dt1$WM_GM2)=="character") dt1$WM_GM2 <-as.numeric(dt1$WM_GM2)
if (class(dt1$DRY_GM2)=="factor") dt1$DRY_GM2 <-as.numeric(levels(dt1$DRY_GM2))[as.integer(dt1$DRY_GM2) ]               
if (class(dt1$DRY_GM2)=="character") dt1$DRY_GM2 <-as.numeric(dt1$DRY_GM2)
if (class(dt1$SFDM)=="factor") dt1$SFDM <-as.numeric(levels(dt1$SFDM))[as.integer(dt1$SFDM) ]               
if (class(dt1$SFDM)=="character") dt1$SFDM <-as.numeric(dt1$SFDM)
if (class(dt1$AFDM)=="factor") dt1$AFDM <-as.numeric(levels(dt1$AFDM))[as.integer(dt1$AFDM) ]               
if (class(dt1$AFDM)=="character") dt1$AFDM <-as.numeric(dt1$AFDM)
if (class(dt1$SCIENTIFIC_NAME)!="factor") dt1$SCIENTIFIC_NAME<- as.factor(dt1$SCIENTIFIC_NAME)
if (class(dt1$COMMON_NAME)!="factor") dt1$COMMON_NAME<- as.factor(dt1$COMMON_NAME)
if (class(dt1$TAXON_KINGDOM)!="factor") dt1$TAXON_KINGDOM<- as.factor(dt1$TAXON_KINGDOM)
if (class(dt1$TAXON_PHYLUM)!="factor") dt1$TAXON_PHYLUM<- as.factor(dt1$TAXON_PHYLUM)
if (class(dt1$TAXON_CLASS)!="factor") dt1$TAXON_CLASS<- as.factor(dt1$TAXON_CLASS)
if (class(dt1$TAXON_ORDER)!="factor") dt1$TAXON_ORDER<- as.factor(dt1$TAXON_ORDER)
if (class(dt1$TAXON_FAMILY)!="factor") dt1$TAXON_FAMILY<- as.factor(dt1$TAXON_FAMILY)
if (class(dt1$TAXON_GENUS)!="factor") dt1$TAXON_GENUS<- as.factor(dt1$TAXON_GENUS)
if (class(dt1$GROUP)!="factor") dt1$GROUP<- as.factor(dt1$GROUP)
if (class(dt1$MOBILITY)!="factor") dt1$MOBILITY<- as.factor(dt1$MOBILITY)
if (class(dt1$GROWTH_MORPH)!="factor") dt1$GROWTH_MORPH<- as.factor(dt1$GROWTH_MORPH)
if (class(dt1$COARSE_GROUPING)!="factor") dt1$COARSE_GROUPING<- as.factor(dt1$COARSE_GROUPING)

# Convert Missing Values to NA for non-dates

dt1$VIS <- ifelse((trimws(as.character(dt1$VIS))==trimws("-99999")),NA,dt1$VIS)               
suppressWarnings(dt1$VIS <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$VIS))==as.character(as.numeric("-99999"))),NA,dt1$VIS))
dt1$PERCENT_COVER <- ifelse((trimws(as.character(dt1$PERCENT_COVER))==trimws("-99999")),NA,dt1$PERCENT_COVER)               
suppressWarnings(dt1$PERCENT_COVER <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$PERCENT_COVER))==as.character(as.numeric("-99999"))),NA,dt1$PERCENT_COVER))
dt1$DENSITY <- ifelse((trimws(as.character(dt1$DENSITY))==trimws("-99999")),NA,dt1$DENSITY)               
suppressWarnings(dt1$DENSITY <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$DENSITY))==as.character(as.numeric("-99999"))),NA,dt1$DENSITY))
dt1$WM_GM2 <- ifelse((trimws(as.character(dt1$WM_GM2))==trimws("-99999")),NA,dt1$WM_GM2)               
suppressWarnings(dt1$WM_GM2 <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$WM_GM2))==as.character(as.numeric("-99999"))),NA,dt1$WM_GM2))
dt1$DRY_GM2 <- ifelse((trimws(as.character(dt1$DRY_GM2))==trimws("-99999")),NA,dt1$DRY_GM2)               
suppressWarnings(dt1$DRY_GM2 <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$DRY_GM2))==as.character(as.numeric("-99999"))),NA,dt1$DRY_GM2))
dt1$SFDM <- ifelse((trimws(as.character(dt1$SFDM))==trimws("-99999")),NA,dt1$SFDM)               
suppressWarnings(dt1$SFDM <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$SFDM))==as.character(as.numeric("-99999"))),NA,dt1$SFDM))
dt1$AFDM <- ifelse((trimws(as.character(dt1$AFDM))==trimws("-99999")),NA,dt1$AFDM)               
suppressWarnings(dt1$AFDM <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$AFDM))==as.character(as.numeric("-99999"))),NA,dt1$AFDM))
dt1$SCIENTIFIC_NAME <- as.factor(ifelse((trimws(as.character(dt1$SCIENTIFIC_NAME))==trimws("-99999")),NA,as.character(dt1$SCIENTIFIC_NAME)))
dt1$TAXON_PHYLUM <- as.factor(ifelse((trimws(as.character(dt1$TAXON_PHYLUM))==trimws("-99999")),NA,as.character(dt1$TAXON_PHYLUM)))
dt1$TAXON_CLASS <- as.factor(ifelse((trimws(as.character(dt1$TAXON_CLASS))==trimws("-99999")),NA,as.character(dt1$TAXON_CLASS)))
dt1$TAXON_ORDER <- as.factor(ifelse((trimws(as.character(dt1$TAXON_ORDER))==trimws("-99999")),NA,as.character(dt1$TAXON_ORDER)))
dt1$TAXON_FAMILY <- as.factor(ifelse((trimws(as.character(dt1$TAXON_FAMILY))==trimws("-99999")),NA,as.character(dt1$TAXON_FAMILY)))

# bm_trans <- dt1 %>% 
#   dplyr::select(YEAR, SITE, TRANSECT, AFDM, SCIENTIFIC_NAME, COARSE_GROUPING) %>% 
#   dplyr::group_by(YEAR, SITE, TRANSECT, COARSE_GROUPING)  %>% 
#   dplyr:: summarise(AFDM = mean(AFDM, na.omit = T))  %>% ungroup()

bm_trans <- dt1 %>% 
  dplyr::select(YEAR, SITE, TRANSECT, AFDM, SCIENTIFIC_NAME, COARSE_GROUPING) %>% 
  dplyr::group_by(YEAR, SITE, TRANSECT, COARSE_GROUPING)  %>% dplyr::summarise(AFDM = sum(AFDM, na.rm = T)) %>% ungroup() %>% dplyr::group_by(YEAR, SITE, TRANSECT, COARSE_GROUPING) %>% 
  dplyr:: summarise(AFDM = mean(AFDM, na.rm = T))  %>% ungroup()

bm_trans1 <- dt1 %>%
  dplyr::select(YEAR, SITE, TRANSECT, AFDM, SCIENTIFIC_NAME, COARSE_GROUPING) %>%
  dplyr::group_by(YEAR, SITE, TRANSECT, COARSE_GROUPING)  %>%
  dplyr:: summarise(AFDM = mean(AFDM, na.rm = T))  %>% ungroup()

bm_trans <- bm_trans %>% mutate(AFDM = AFDM + 0.0001)
bm_trans <- as.data.frame(bm_trans) 



bm_trans2 <- bm_trans %>%
  pivot_wider(names_from = COARSE_GROUPING, values_from = AFDM)

wave_data <- read.csv("./Wavekelpthesis/Edited_data/mod_wave_subset.csv")
wave_data <- wave_data[,c("site", "Max_Hs_m", "wave_yr")]
wave_data$WAVE_YR <- wave_data$wave_yr
wave_data <- wave_data %>% rename(SITE = site, MAX_HS_M = Max_Hs_m)
if (class(wave_data$SITE)!="factor") wave_data$SITE<- as.factor(wave_data$SITE)
wave_data$MAX_HS_M <- as.numeric(wave_data$MAX_HS_M)
# if (class(wave_data$MAX_HS_M)!="factor") wave_data$MAX_HS_M<- as.numeric(levels(wave_data$MAX_HS_M))[as.integer(wave_data$MAX_HS_M) ]       
wave_data <- wave_data[,c("SITE", "MAX_HS_M", "WAVE_YR")]
wave_data <- as.data.frame(wave_data)

bm_wave_merge <- left_join(bm_trans2, wave_data, by = c("YEAR" = "WAVE_YR", "SITE" = "SITE"))
# bm_wave_merge <- merge(y = bm_site2, x = wave_data, by.y = c("SITE","YEAR"), by.x = c("SITE", "WAVE_YR"), all = TRUE)

not_in_merged_df <- bm_trans2 %>% filter(!SITE %in% bm_wave_merge$SITE)

bm_wave_merge <- bm_wave_merge %>% filter(!SITE %in% c('SCDI', 'SCTW'))
bm_wave_merge <- bm_wave_merge %>% rename(WAVE_YR = YEAR)

#subset by functional groups
bm_wave_sand <- left_join(bm_wave_merge, sand_tran, by = c("WAVE_YR" = "YEAR", "SITE" = "SITE", "TRANSECT" = "TRANSECT"))
not_in_merged_sand <- bm_wave_merge %>% filter(!SITE %in% bm_wave_sand$SITE)

mobile_invert <- bm_wave_sand %>% dplyr::select(SITE, WAVE_YR, TRANSECT, `MOBILE INVERT`, MAX_HS_M, SAND_TOTAL, SAND_SS )
epi_sessile_invert <- bm_wave_sand %>% dplyr::select(SITE, WAVE_YR, TRANSECT, `EPILITHIC SESSILE INVERT`, MAX_HS_M, SAND_TOTAL, SAND_SS )
endo_sessile_invert <- bm_wave_sand %>% dplyr::select(SITE, WAVE_YR, TRANSECT, `ENDOLITHIC SESSILE INVERT`, MAX_HS_M, SAND_TOTAL, SAND_SS )
understory_algae <- bm_wave_sand %>% dplyr::select(SITE, WAVE_YR, TRANSECT, `UNDERSTORY ALGAE`, MAX_HS_M, SAND_TOTAL, SAND_SS )
giant_kelp <- bm_wave_sand %>% dplyr::select(SITE, WAVE_YR, TRANSECT, `GIANT KELP`, MAX_HS_M, SAND_TOTAL, SAND_SS )
fish <- bm_wave_sand %>% dplyr::select(SITE, WAVE_YR, TRANSECT, `FISH`, MAX_HS_M, SAND_TOTAL,SAND_SS )

mobile_invert <- as.data.frame(mobile_invert)
epi_sessile_invert <- as.data.frame(epi_sessile_invert)
endo_sessile_invert <- as.data.frame(endo_sessile_invert)
understory_algae <- as.data.frame(understory_algae)
giant_kelp <- as.data.frame(giant_kelp)
fish <- as.data.frame(fish)

pdf(file = "./Wavekelpthesis/Code/LTER data/Biomass/11-13-24_biomasstrans_simplelm.pdf", paper = "us")
#simple linear regressions
lr_func <- function(biomass_subset){
  biomass_subset_lin_reg <- lm(data= biomass_subset,formula = biomass_subset[,4]~(biomass_subset$MAX_HS_M))
  biomass_subset_lin_reg_plot <- plot(y= biomass_subset[,4],x=as.numeric(biomass_subset$MAX_HS_M),type="p", xlab = "Max wave height (m)", ylab = "biomass per site/year (g)", main =  glue("Max winter wave height vs \n{colnames(biomass_subset[4])} biomass TRANSECT LEVEL"))
  biomass_subset_prt_intercept <- coef(summary(biomass_subset_lin_reg))[1,'t value']
  biomass_subset_prt_waves <- coef(summary(biomass_subset_lin_reg))[2,'t value']
  biomass_subset_rsquared <- summary(biomass_subset_lin_reg)$r.squared
  biomass_subset_fvalue <- summary(biomass_subset_lin_reg)$fstatistic[1]
  biomass_subset_pvalue <- pf(summary(biomass_subset_lin_reg)$fstatistic[1],summary(biomass_subset_lin_reg)$fstatistic[2],summary(biomass_subset_lin_reg)$fstatistic[3],lower.tail = FALSE)
  abline(biomass_subset_lin_reg$coefficients[1],biomass_subset_lin_reg$coefficients[2], col = "red")
  text(x = 4.9, y = max(biomass_subset[4])-(max(biomass_subset[4])*0.2), label = glue("Pr(>|t|) = {round(biomass_subset_prt_intercept,digits = 6)} (intercept) \nPr(>|t|) = {round(biomass_subset_prt_waves,digits = 6)} (waves) \nr-squared = {round(biomass_subset_rsquared,digits = 6)} \nF-value = {round(biomass_subset_fvalue,digits = 6)} \np-value = {round(biomass_subset_pvalue,digits = 6)}"), cex = 0.5, pos = 2)
  return(biomass_subset_lin_reg)
}


lr_func(mobile_invert)
lr_func(epi_sessile_invert)
lr_func(endo_sessile_invert)
lr_func(understory_algae)
lr_func(giant_kelp)
lr_func(fish)

dev.off()

mobile_invert <- as.data.frame(mobile_invert)

#GLMMTMB
# biomass_glmmtmb_transavg <- merge(x = mobile_invert, y = rock_site_avgs_1, by.y = c("SITE","YEAR"), by.x =  c("site","wave_yr"))
# year.factor <- (biomass_glmmtmb_transavg$wave_yr)

#just sand
sink("./Wavekelpthesis/Code/LTER data/Biomass/11-15-24_biomasstrans_glmmtmb.txt")

mobile_invert_glmmTMB <- glmmTMB(mobile_invert$`MOBILE INVERT`~mobile_invert$MAX_HS_M+mobile_invert$SAND_TOTAL+ ar1(as.factor(mobile_invert$WAVE_YR)+0|mobile_invert$SITE),family = Gamma(link = "log"))
print(summary(mobile_invert_glmmTMB))
epi_sessile_invert_glmmTMB <- glmmTMB(epi_sessile_invert$`EPILITHIC SESSILE INVERT`~epi_sessile_invert$MAX_HS_M+epi_sessile_invert$SAND_TOTAL+ ar1(as.factor(epi_sessile_invert$WAVE_YR)+0|epi_sessile_invert$SITE),family = Gamma(link = "log"))
print(summary(epi_sessile_invert_glmmTMB))
endo_sessile_invert_glmmTMB <- glmmTMB(endo_sessile_invert$`ENDOLITHIC SESSILE INVERT`~endo_sessile_invert$MAX_HS_M+endo_sessile_invert$SAND_TOTAL+ ar1(as.factor(endo_sessile_invert$WAVE_YR)+0|endo_sessile_invert$SITE),family = Gamma(link = "log"))
print(summary(endo_sessile_invert_glmmTMB))
understory_algae_glmmTMB <- glmmTMB(understory_algae$`UNDERSTORY ALGAE`~understory_algae$MAX_HS_M+understory_algae$SAND_TOTAL+ ar1(as.factor(understory_algae$WAVE_YR)+0|understory_algae$SITE),family = Gamma(link = "log"))
print(summary(understory_algae_glmmTMB))
giant_kelp_glmmTMB <- glmmTMB(giant_kelp$`GIANT KELP`~giant_kelp$MAX_HS_M+giant_kelp$SAND_TOTAL+ ar1(as.factor(giant_kelp$WAVE_YR)+0|giant_kelp$SITE),family = Gamma(link = "log"))
print(summary(giant_kelp_glmmTMB))
fish_glmmTMB <- glmmTMB(fish$`FISH`~fish$MAX_HS_M+fish$SAND_TOTAL+ ar1(as.factor(fish$WAVE_YR)+0|fish$SITE),family = Gamma(link = "log"))
print(summary(fish_glmmTMB))

sink()

#sand+shallow sand
sink("./Wavekelpthesis/Code/LTER data/Biomass/11-17-24_biomasstrans_glmmtmb_ss.txt")
mobile_invert_glmmTMB <- glmmTMB(mobile_invert$`MOBILE INVERT`~mobile_invert$MAX_HS_M+mobile_invert$SAND_SS+ ar1(as.factor(mobile_invert$WAVE_YR)+0|mobile_invert$SITE),family = Gamma(link = "log"))
print(summary(mobile_invert_glmmTMB))
epi_sessile_invert_glmmTMB <- glmmTMB(epi_sessile_invert$`EPILITHIC SESSILE INVERT`~epi_sessile_invert$MAX_HS_M+epi_sessile_invert$SAND_SS+ ar1(as.factor(epi_sessile_invert$WAVE_YR)+0|epi_sessile_invert$SITE),family = Gamma(link = "log"))
print(summary(epi_sessile_invert_glmmTMB))
endo_sessile_invert_glmmTMB <- glmmTMB(endo_sessile_invert$`ENDOLITHIC SESSILE INVERT`~endo_sessile_invert$MAX_HS_M+endo_sessile_invert$SAND_SS+ ar1(as.factor(endo_sessile_invert$WAVE_YR)+0|endo_sessile_invert$SITE),family = Gamma(link = "log"))
print(summary(endo_sessile_invert_glmmTMB))
understory_algae_glmmTMB <- glmmTMB(understory_algae$`UNDERSTORY ALGAE`~understory_algae$MAX_HS_M+understory_algae$SAND_SS+ ar1(as.factor(understory_algae$WAVE_YR)+0|understory_algae$SITE),family = Gamma(link = "log"))
print(summary(understory_algae_glmmTMB))
giant_kelp_glmmTMB <- glmmTMB(giant_kelp$`GIANT KELP`~giant_kelp$MAX_HS_M+giant_kelp$SAND_SS+ ar1(as.factor(giant_kelp$WAVE_YR)+0|giant_kelp$SITE),family = Gamma(link = "log"))
print(summary(giant_kelp_glmmTMB))
fish_glmmTMB <- glmmTMB(fish$`FISH`~fish$MAX_HS_M+fish$SAND_SS+ ar1(as.factor(fish$WAVE_YR)+0|fish$SITE),family = Gamma(link = "log"))
print(summary(fish_glmmTMB))

sink()