rm(list = ls())
rm(list = ls())
library(dplyr)
library(lubridate)
library(tidyr)
library(glue)
library(glmmTMB)

# Package ID: knb-lter-sbc.15.35 Cataloging System:https://pasta.edirepository.org.
# Data set title: SBC LTER: Reef: Kelp Forest Community Dynamics: Cover of sessile organisms, Uniform Point Contact.
# Data set creator:  Daniel C Reed -  
# Data set creator:  Robert J Miller -  
# Contact:    -  Santa Barbara Coastal LTER  - sbclter@msi.ucsb.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-sbc/15/35/75eec9fc9a1bde5a7de6b141c8910ba7" 
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
                 "SP_CODE",     
                 "PERCENT_COVER",     
                 "SCIENTIFIC_NAME",     
                 "COMMON_NAME",     
                 "TAXON_KINGDOM",     
                 "TAXON_PHYLUM",     
                 "TAXON_CLASS",     
                 "TAXON_ORDER",     
                 "TAXON_FAMILY",     
                 "TAXON_GENUS",     
                 "GROUP",     
                 "SURVEY",     
                 "MOBILITY",     
                 "GROWTH_MORPH"    ), check.names=TRUE)

unlink(infile1)

dt1$COARSE_GROUPING <- paste(dt1$MOBILITY,dt1$GROUP)
dt1$QUAD_SIDE <- paste(dt1$QUAD, dt1$SIDE)

for(animal in 1:length(dt1$SCIENTIFIC_NAME))
{
  if(dt1$SCIENTIFIC_NAME[animal] == "Parapholas californica" | dt1$SCIENTIFIC_NAME[animal] == "Chaceia ovoidea" )
  {
    dt1$COARSE_GROUPING[animal] <- "ENDOLITHIC SESSILE INVERT"
  }
}

for(animal in 1:length(dt1$COARSE_GROUPING))
{
  if(dt1$COARSE_GROUPING[animal] == "SESSILE INVERT" | dt1$COARSE_GROUPING[animal] == "MOBILE INVERT")
  {
    dt1$COARSE_GROUPING[animal] <- "EPILITHIC SESSILE INVERT"
  }
}

for(animal in 1:length(dt1$SCIENTIFIC_NAME))
{
  if(dt1$SCIENTIFIC_NAME[animal] == "Macrocystis pyrifera")
  {
    dt1$COARSE_GROUPING[animal] <- "GIANT KELP"
  }
}


for(animal in 1:length(dt1$COARSE_GROUPING))
{
  if(dt1$COARSE_GROUPING[animal] == "SESSILE ALGAE" | dt1$COARSE_GROUPING[animal] == "SESSILE PLANT" )
  {
    dt1$COARSE_GROUPING[animal] <- "UNDERSTORY ALGAE"
  }
}

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

# attempting to convert dt1$DATE dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1DATE<-as.Date(dt1$DATE,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt1[dt1$DATE != "",]) == length(tmp1DATE[!is.na(tmp1DATE)])){dt1$DATE <- tmp1DATE } else {print("Date conversion failed for dt1$DATE. Please inspect the data and do the date conversion yourself.")}    



if (class(dt1$SITE)!="factor") dt1$SITE<- as.factor(dt1$SITE)
if (class(dt1$TRANSECT)!="factor") dt1$TRANSECT<- as.factor(dt1$TRANSECT)
if (class(dt1$QUAD)!="factor") dt1$QUAD<- as.factor(dt1$QUAD)
if (class(dt1$SIDE)!="factor") dt1$SIDE<- as.factor(dt1$SIDE)
if (class(dt1$SP_CODE)!="factor") dt1$SP_CODE<- as.factor(dt1$SP_CODE)
if (class(dt1$PERCENT_COVER)=="factor") dt1$PERCENT_COVER <-as.numeric(levels(dt1$PERCENT_COVER))[as.integer(dt1$PERCENT_COVER) ]               
if (class(dt1$PERCENT_COVER)=="character") dt1$PERCENT_COVER <-as.numeric(dt1$PERCENT_COVER)
if (class(dt1$SCIENTIFIC_NAME)!="factor") dt1$SCIENTIFIC_NAME<- as.factor(dt1$SCIENTIFIC_NAME)
if (class(dt1$COMMON_NAME)!="factor") dt1$COMMON_NAME<- as.factor(dt1$COMMON_NAME)
if (class(dt1$TAXON_KINGDOM)!="factor") dt1$TAXON_KINGDOM<- as.factor(dt1$TAXON_KINGDOM)
if (class(dt1$TAXON_PHYLUM)!="factor") dt1$TAXON_PHYLUM<- as.factor(dt1$TAXON_PHYLUM)
if (class(dt1$TAXON_CLASS)!="factor") dt1$TAXON_CLASS<- as.factor(dt1$TAXON_CLASS)
if (class(dt1$TAXON_ORDER)!="factor") dt1$TAXON_ORDER<- as.factor(dt1$TAXON_ORDER)
if (class(dt1$TAXON_FAMILY)!="factor") dt1$TAXON_FAMILY<- as.factor(dt1$TAXON_FAMILY)
if (class(dt1$TAXON_GENUS)!="factor") dt1$TAXON_GENUS<- as.factor(dt1$TAXON_GENUS)
if (class(dt1$GROUP)!="factor") dt1$GROUP<- as.factor(dt1$GROUP)
if (class(dt1$SURVEY)!="factor") dt1$SURVEY<- as.factor(dt1$SURVEY)
if (class(dt1$MOBILITY)!="factor") dt1$MOBILITY<- as.factor(dt1$MOBILITY)
if (class(dt1$GROWTH_MORPH)!="factor") dt1$GROWTH_MORPH<- as.factor(dt1$GROWTH_MORPH)
if (class(dt1$COARSE_GROUPING)!="factor") dt1$COARSE_GROUPING<- as.factor(dt1$COARSE_GROUPING)
if (class(dt1$QUAD_SIDE)!="factor") dt1$QUAD_SIDE<- as.factor(dt1$QUAD_SIDE)
# Convert Missing Values to NA for non-dates

dt1$SITE <- as.factor(ifelse((trimws(as.character(dt1$SITE))==trimws("-99999")),NA,as.character(dt1$SITE)))
dt1$TRANSECT <- as.factor(ifelse((trimws(as.character(dt1$TRANSECT))==trimws("-99999")),NA,as.character(dt1$TRANSECT)))
dt1$QUAD <- as.factor(ifelse((trimws(as.character(dt1$QUAD))==trimws("-99999")),NA,as.character(dt1$QUAD)))
dt1$SIDE <- as.factor(ifelse((trimws(as.character(dt1$SIDE))==trimws("-99999")),NA,as.character(dt1$SIDE)))
dt1$SP_CODE <- as.factor(ifelse((trimws(as.character(dt1$SP_CODE))==trimws("-99999")),NA,as.character(dt1$SP_CODE)))
dt1$PERCENT_COVER <- ifelse((trimws(as.character(dt1$PERCENT_COVER))==trimws("-99999")),NA,dt1$PERCENT_COVER)               
suppressWarnings(dt1$PERCENT_COVER <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$PERCENT_COVER))==as.character(as.numeric("-99999"))),NA,dt1$PERCENT_COVER))
dt1$SCIENTIFIC_NAME <- as.factor(ifelse((trimws(as.character(dt1$SCIENTIFIC_NAME))==trimws("-99999")),NA,as.character(dt1$SCIENTIFIC_NAME)))
dt1$COMMON_NAME <- as.factor(ifelse((trimws(as.character(dt1$COMMON_NAME))==trimws("-99999")),NA,as.character(dt1$COMMON_NAME)))
dt1$TAXON_KINGDOM <- as.factor(ifelse((trimws(as.character(dt1$TAXON_KINGDOM))==trimws("-99999")),NA,as.character(dt1$TAXON_KINGDOM)))
dt1$TAXON_PHYLUM <- as.factor(ifelse((trimws(as.character(dt1$TAXON_PHYLUM))==trimws("-99999")),NA,as.character(dt1$TAXON_PHYLUM)))
dt1$TAXON_CLASS <- as.factor(ifelse((trimws(as.character(dt1$TAXON_CLASS))==trimws("-99999")),NA,as.character(dt1$TAXON_CLASS)))
dt1$TAXON_ORDER <- as.factor(ifelse((trimws(as.character(dt1$TAXON_ORDER))==trimws("-99999")),NA,as.character(dt1$TAXON_ORDER)))
dt1$TAXON_FAMILY <- as.factor(ifelse((trimws(as.character(dt1$TAXON_FAMILY))==trimws("-99999")),NA,as.character(dt1$TAXON_FAMILY)))
dt1$TAXON_GENUS <- as.factor(ifelse((trimws(as.character(dt1$TAXON_GENUS))==trimws("-99999")),NA,as.character(dt1$TAXON_GENUS)))
dt1$GROUP <- as.factor(ifelse((trimws(as.character(dt1$GROUP))==trimws("-99999")),NA,as.character(dt1$GROUP)))
dt1$SURVEY <- as.factor(ifelse((trimws(as.character(dt1$SURVEY))==trimws("-99999")),NA,as.character(dt1$SURVEY)))
dt1$MOBILITY <- as.factor(ifelse((trimws(as.character(dt1$MOBILITY))==trimws("-99999")),NA,as.character(dt1$MOBILITY)))
dt1$GROWTH_MORPH <- as.factor(ifelse((trimws(as.character(dt1$GROWTH_MORPH))==trimws("-99999")),NA,as.character(dt1$GROWTH_MORPH)))
dt1$QUAD_SIDE <- as.factor(ifelse((trimws(as.character(dt1$QUAD_SIDE))==trimws("-99999 -99999")),NA,as.character(dt1$QUAD_SIDE)))

pc_site <- dt1 %>% dplyr:: select(YEAR, SITE, TRANSECT, QUAD_SIDE, PERCENT_COVER, SCIENTIFIC_NAME, COARSE_GROUPING) %>% dplyr::group_by(YEAR, SITE, TRANSECT, QUAD_SIDE, COARSE_GROUPING) %>% dplyr:: summarise(PERCENT_COVER = sum(PERCENT_COVER, na.rm = TRUE)) %>% ungroup() %>% dplyr:: group_by(YEAR, SITE, TRANSECT, COARSE_GROUPING) %>% dplyr:: summarise(PERCENT_COVER = mean(PERCENT_COVER, na.rm = TRUE)) %>% ungroup() %>% dplyr:: group_by(YEAR, SITE, COARSE_GROUPING) %>% dplyr:: summarise(PERCENT_COVER = mean(PERCENT_COVER, na.rm = TRUE)) %>% ungroup()
pc_site <- pc_site %>% mutate(PERCENT_COVER= PERCENT_COVER + 0.0001)
pc_site <- as.data.frame(pc_site) 

pc_site2 <- pc_site %>%
  pivot_wider(names_from = COARSE_GROUPING, values_from = PERCENT_COVER)

wave_data <- read.csv("./Wavekelpthesis/Edited_data/mod_wave_subset.csv")
wave_data <- wave_data[,c("site", "Max_Hs_m", "wave_yr")]
wave_data$WAVE_YR <- wave_data$wave_yr
wave_data <- wave_data %>% rename(SITE = site, MAX_HS_M = Max_Hs_m)
if (class(wave_data$SITE)!="factor") wave_data$SITE<- as.factor(wave_data$SITE)
wave_data$MAX_HS_M <- as.numeric(wave_data$MAX_HS_M)
wave_data <- wave_data[,c("SITE", "MAX_HS_M", "WAVE_YR")]
wave_data <- as.data.frame(wave_data)


pc_wave_merge <- left_join(pc_site2, wave_data, by = c("YEAR" = "WAVE_YR", "SITE" = "SITE"))
# bm_wave_merge <- merge(y = bm_site2, x = wave_data, by.y = c("SITE","YEAR"), by.x = c("SITE", "WAVE_YR"), all = TRUE)

not_in_merged_df <- pc_site2 %>% filter(!SITE %in% pc_wave_merge$SITE)

#ask!!

pc_wave_merge <- pc_wave_merge %>% filter(!SITE %in% c('SCDI', 'SCTW'))
pc_wave_merge <- pc_wave_merge %>% rename(WAVE_YR = YEAR)

pc_wave_sand <- left_join(pc_wave_merge, sand_site, by = c("WAVE_YR" = "YEAR", "SITE" = "SITE"))
not_in_merged_sand <- pc_wave_merge %>% filter(!SITE %in% pc_wave_sand$SITE)

epi_sessile_invert <- pc_wave_sand %>% dplyr::select(SITE, WAVE_YR, `EPILITHIC SESSILE INVERT`, MAX_HS_M, SAND_TOTAL, SAND_SS )
endo_sessile_invert <- pc_wave_sand %>% dplyr::select(SITE, WAVE_YR,`ENDOLITHIC SESSILE INVERT`, MAX_HS_M, SAND_TOTAL, SAND_SS )
understory_algae <- pc_wave_sand %>% dplyr::select(SITE, WAVE_YR, `UNDERSTORY ALGAE`, MAX_HS_M, SAND_TOTAL, SAND_SS )
giant_kelp <- pc_wave_sand %>% dplyr::select(SITE, WAVE_YR, `GIANT KELP`, MAX_HS_M, SAND_TOTAL, SAND_SS )

epi_sessile_invert <- as.data.frame(epi_sessile_invert)
endo_sessile_invert <- as.data.frame(endo_sessile_invert)
understory_algae <- as.data.frame(understory_algae)
giant_kelp <- as.data.frame(giant_kelp)

pdf(file = "./Wavekelpthesis/Code/LTER data/Percent cover/11-14-24_pcsite_simplelm.pdf", paper = "us")
lr_func <- function(pc_subset){
  pc_subset_lin_reg <- lm(data= pc_subset,formula = pc_subset[,3]~pc_subset$MAX_HS_M)
  pc_subset_lin_reg_plot <- plot(y= pc_subset[,3],x=pc_subset$MAX_HS_M,type="p", xlab = "Max wave height (m)", ylab = "pc per site/year (g)", main =  glue("Max winter wave height vs \n{colnames(pc_subset[3])} percent cover SITE"))
  pc_subset_prt_intercept <- coef(summary(pc_subset_lin_reg))[1,'t value']
  pc_subset_prt_waves <- coef(summary(pc_subset_lin_reg))[2,'t value']
  pc_subset_rsquared <- summary(pc_subset_lin_reg)$r.squared
  pc_subset_fvalue <- summary(pc_subset_lin_reg)$fstatistic[1]
  pc_subset_pvalue <- pf(summary(pc_subset_lin_reg)$fstatistic[1],summary(pc_subset_lin_reg)$fstatistic[2],summary(pc_subset_lin_reg)$fstatistic[3],lower.tail = FALSE)
  abline(pc_subset_lin_reg$coefficients[1],pc_subset_lin_reg$coefficients[2], col = "red")
  text(x = 4.9, y = max(pc_subset[3])-(max(pc_subset[3])*0.2), label = glue("Pr(>|t|) = {round(pc_subset_prt_intercept,digits = 6)} (intercept) \nPr(>|t|) = {round(pc_subset_prt_waves,digits = 6)} (waves) \nr-squared = {round(pc_subset_rsquared,digits = 6)} \nF-value = {round(pc_subset_fvalue,digits = 6)} \np-value = {round(pc_subset_pvalue,digits = 6)}"), cex = 0.5, pos = 2)
  return(pc_subset_lin_reg_plot)
}
dev.off()

lr_func(epi_sessile_invert)
lr_func(endo_sessile_invert)
lr_func(understory_algae)
lr_func(giant_kelp)

sink("./Wavekelpthesis/Code/LTER data/Percent cover/11-15-24_pcsite_glmmtmb.txt")
epi_sessile_invert_glmmTMB <- glmmTMB(epi_sessile_invert$`EPILITHIC SESSILE INVERT`~epi_sessile_invert$MAX_HS_M  + epi_sessile_invert$SAND_TOTAL+ ar1(as.factor(epi_sessile_invert$WAVE_YR)+0|epi_sessile_invert$SITE),family = Gamma(link = "log"), data = epi_sessile_invert )
print(summary(epi_sessile_invert_glmmTMB))

endo_sessile_invert_glmmTMB <- glmmTMB(endo_sessile_invert$`ENDOLITHIC SESSILE INVERT`~endo_sessile_invert$MAX_HS_M + endo_sessile_invert$SAND_TOTAL+ ar1(as.factor(endo_sessile_invert$WAVE_YR)+0|endo_sessile_invert$SITE),family = Gamma(link = "log"), data = endo_sessile_invert )
print(summary(endo_sessile_invert_glmmTMB))

understory_algae_glmmTMB <- glmmTMB(understory_algae$`UNDERSTORY ALGAE`~understory_algae$MAX_HS_M + understory_algae$SAND_TOTAL+ ar1(as.factor(understory_algae$WAVE_YR)+0|understory_algae$SITE),family = Gamma(link = "log"), data = understory_algae)
print(summary(understory_algae_glmmTMB))

giant_kelp_glmmTMB <- glmmTMB(giant_kelp$`GIANT KELP`~giant_kelp$MAX_HS_M + giant_kelp$SAND_TOTAL+ ar1(as.factor(giant_kelp$WAVE_YR)+0|giant_kelp$SITE),family = Gamma(link = "log"), data = giant_kelp )
print(summary(giant_kelp_glmmTMB))
sink()

#shallow sand + sand
sink("./Wavekelpthesis/Code/LTER data/Percent cover/11-15-24_pcsite_glmmtmb_ss.txt")
epi_sessile_invert_glmmTMB <- glmmTMB(epi_sessile_invert$`EPILITHIC SESSILE INVERT`~epi_sessile_invert$MAX_HS_M  + epi_sessile_invert$SAND_SS+ ar1(as.factor(epi_sessile_invert$WAVE_YR)+0|epi_sessile_invert$SITE),family = Gamma(link = "log"), data = epi_sessile_invert )
print(summary(epi_sessile_invert_glmmTMB))

endo_sessile_invert_glmmTMB <- glmmTMB(endo_sessile_invert$`ENDOLITHIC SESSILE INVERT`~endo_sessile_invert$MAX_HS_M + endo_sessile_invert$SAND_SS+ ar1(as.factor(endo_sessile_invert$WAVE_YR)+0|endo_sessile_invert$SITE),family = Gamma(link = "log"), data = endo_sessile_invert )
print(summary(endo_sessile_invert_glmmTMB))

understory_algae_glmmTMB <- glmmTMB(understory_algae$`UNDERSTORY ALGAE`~understory_algae$MAX_HS_M + understory_algae$SAND_SS+ ar1(as.factor(understory_algae$WAVE_YR)+0|understory_algae$SITE),family = Gamma(link = "log"), data = understory_algae)
print(summary(understory_algae_glmmTMB))

giant_kelp_glmmTMB <- glmmTMB(giant_kelp$`GIANT KELP`~giant_kelp$MAX_HS_M + giant_kelp$SAND_SS+ ar1(as.factor(giant_kelp$WAVE_YR)+0|giant_kelp$SITE),family = Gamma(link = "log"), data = giant_kelp )
print(summary(giant_kelp_glmmTMB))
sink()
