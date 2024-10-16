rm(list = ls())
rm(list = ls())
library(dplyr)
library(lubridate)
library(tidyr)
library(glue)

#read in wave raw data
setwd("/Users/aleynaloughran-pierce/Desktop")
LTER_data <- read.csv("./Wavekelpthesis/Raw Files/Annual_All_Species_Biomass_at_transect_20240823.csv")
#summer months range from early july to late october (month = 7: 10)

#sorts out columns into year, site, biomass, scientific name, and coarse grouping
biomass_data <- LTER_data[,c("YEAR", "SITE", "TRANSECT", "AFDM", "SCIENTIFIC_NAME", "COARSE_GROUPING")]

#if the biomass is missing, replace it with na
for(animal in 1:length(biomass_data$AFDM))
{
  if(biomass_data$AFDM[animal] == -99999.00000)
  {
    biomass_data$AFDM[animal] = NA
  }
}

#boring clam --> endolithic sessile invert
for(animal in 1:length(biomass_data$SCIENTIFIC_NAME))
{
  if(biomass_data$SCIENTIFIC_NAME[animal] == "Parapholas californica" | LTER_data$SCIENTIFIC_NAME[animal] == "Chaceia ovoidea" )
  {
    biomass_data$COARSE_GROUPING[animal] <- "ENDOLITHIC SESSILE INVERT"
  }
}

for(animal in 1:length(biomass_data$COARSE_GROUPING))
{
  if(biomass_data$COARSE_GROUPING[animal] == "SESSILE INVERT")
  {
    biomass_data$COARSE_GROUPING[animal] <- "EPILITHIC SESSILE INVERT"
  }
}
biomass_avgs <- matrix(NA, dim(biomass_data)[1], dim(biomass_data)[2]-2) #make blank matrix with same number of rows but less columns (minus 2)
colnames(biomass_avgs) <- colnames(biomass_data)[c(-3,-5)] #assign column names to new matrix minus columns 3 and 5

Year_vector <- unique(biomass_data$YEAR) #make unique vector based off of year column
Site_vector <- unique(biomass_data$SITE) #make unique vector based off of site column
COARSE_GROUPING_vector <- unique(biomass_data$COARSE_GROUPING) #make unique vector based off of coarse grouping

row_new <- 1 #number of rows added

for(i in Site_vector) #for element in site vector
{
  biomass_stuff <- biomass_data[LTER_data$SITE == i,] #new matrix for everything lining up with site column
  for(j in Year_vector) #for all years
  {
    
    biomass_thing <- biomass_stuff[biomass_stuff$YEAR == j,] #make matrix assigning everything by year
    
    mtrans <- length(unique(biomass_thing$TRANSECT)) #number of transects = unique number of transect entries per year
    
    blank <- all(is.na(biomass_thing$AFDM)) #everything that is na in biomass
    
    for(k in COARSE_GROUPING_vector) #for everything in the different functional groups
    {
      biomass_avgs[row_new,1] <- j #column 1 assigns to the year with new data entry k
      biomass_avgs[row_new,2] <- i #column 2 assigns new row to column entry i
      biomass_avgs[row_new,4] <- k #column 4 assigns new row to column entry k
      
      biomass_knacks <- biomass_thing[biomass_thing$COARSE_GROUPING == k,]  #assigns by coarse grouping
      
      out <- (sum(biomass_knacks$AFDM, na.rm = TRUE))/mtrans #averages things in biomass column by number of transects and gets rid of nas
      
      if(length(out)==0) #if blank
      {
        out <- NA #assign na
        if(blank==FALSE) #if not blank
        {
          out <- 0 #then give it 0
        }
      }
      
      
      biomass_avgs[row_new,3] <- out #enter everything into the third column
      
      row_new <- row_new+1 #iterate by adding additional row
    }
  }
}

biomass_avgs[-which(is.na(biomass_avgs[,3]))] #get rid of all rows which has a biomass of nas

biomass_avgs <- as.data.frame(biomass_avgs) #make into dataframe

biomass_avgs2 <- biomass_avgs %>%
  pivot_wider(names_from = COARSE_GROUPING, values_from = AFDM) #https://stackoverflow.com/questions/64032333/change-column-data-into-column-names
#Draws data from the biomass columns and pivots table so that it's all in columns for each functional group
#ask about error

biomass_avgs2 <- biomass_avgs2[,-9]
#gets rid of null columns
biomass_avgs2 <- biomass_avgs2[-(length(biomass_avgs2$YEAR)),]

#enter in wave data
wave_data <- read.csv("./Wavekelpthesis/Edited_data/mod_wave_subset.csv")
wave_data <- wave_data[,c("site", "Max_Hs_m", "wave_yr")]

#merge wave data and biomass by "site" and "year/wave year" columns
biomass_comparison_all <- merge(y = biomass_avgs2, x = wave_data, by.y = c("SITE","YEAR"), by.x = c("site", "wave_yr"))

#verify reason why biomass_comparison_all is 50 obs less is because of channel island data deletions
not_in_merged_df <- biomass_avgs2 %>% filter(!SITE %in% biomass_comparison_all$site)

#convert to numeric data
for(rcounter in 2:dim(biomass_comparison_all)[2])
{
  biomass_comparison_all[[rcounter]] <- as.numeric(biomass_comparison_all[[rcounter]])
}

#convert all Nans to NAs
for(i in 1:dim(biomass_comparison_all)[1]){
  for(j in 4:dim(biomass_comparison_all)[2])
  {
    if(is.nan(biomass_comparison_all[i,j]) == TRUE)
    {
      biomass_comparison_all[i,j] <- NA
    } else {
      next
    }
  }
}

biomass_comparison_all <- biomass_comparison_all %>% filter(is.na(`MOBILE INVERT`) %in% biomass_comparison_all$`MOBILE INVERT`)

#subset by functional groups
mobile_invert = biomass_comparison_all[,c("site","wave_yr","MOBILE INVERT","Max_Hs_m")]
epi_sessile_invert = biomass_comparison_all[,c("site","wave_yr","EPILITHIC SESSILE INVERT","Max_Hs_m")]
endo_sessile_invert = biomass_comparison_all[,c("site","wave_yr","ENDOLITHIC SESSILE INVERT","Max_Hs_m")]
understory_algae = biomass_comparison_all[,c("site","wave_yr","UNDERSTORY ALGAE","Max_Hs_m")]
giant_kelp = biomass_comparison_all[,c("site","wave_yr","GIANT KELP","Max_Hs_m")]
fish = biomass_comparison_all[,c("site","wave_yr","FISH","Max_Hs_m")]


pdf(file = "./Wavekelpthesis/Code/LTER data/Biomass/10-14-24_biomass_simplelm.pdf", paper = "us")
#simple linear regressions
lr_func <- function(biomass_subset){
  biomass_subset_lin_reg <- lm(data= biomass_subset,formula = biomass_subset[,3]~biomass_subset$Max_Hs_m)
  biomass_subset_lin_reg_plot <- plot(y= biomass_subset[,3],x=biomass_subset$Max_Hs_m,type="p", xlab = "Max wave height (m)", ylab = "biomass per site/year (g)", main =  glue("Max winter wave height vs \n{colnames(biomass_subset[3])} biomass"))
  biomass_subset_prt_intercept <- coef(summary(biomass_subset_lin_reg))[1,'t value']
  biomass_subset_prt_waves <- coef(summary(biomass_subset_lin_reg))[2,'t value']
  biomass_subset_rsquared <- summary(biomass_subset_lin_reg)$r.squared
  biomass_subset_fvalue <- summary(biomass_subset_lin_reg)$fstatistic[1]
  biomass_subset_pvalue <- pf(summary(biomass_subset_lin_reg)$fstatistic[1],summary(biomass_subset_lin_reg)$fstatistic[2],summary(biomass_subset_lin_reg)$fstatistic[3],lower.tail = FALSE)
  abline(biomass_subset_lin_reg$coefficients[1],biomass_subset_lin_reg$coefficients[2], col = "red")
  text(x = 4.9, y = max(biomass_subset[3])-(max(biomass_subset[3])*0.2), label = glue("Pr(>|t|) = {round(biomass_subset_prt_intercept,digits = 6)} (intercept) \nPr(>|t|) = {round(biomass_subset_prt_waves,digits = 6)} (waves) \nr-squared = {round(biomass_subset_rsquared,digits = 6)} \nF-value = {round(biomass_subset_fvalue,digits = 6)} \np-value = {round(biomass_subset_pvalue,digits = 6)}"), cex = 0.5, pos = 2)
  return(biomass_subset_lin_reg_plot)
}

lr_func(mobile_invert)
lr_func(epi_sessile_invert)
lr_func(endo_sessile_invert)
lr_func(understory_algae)
lr_func(giant_kelp)
lr_func(fish)

dev.off()

#GLMMTMB









