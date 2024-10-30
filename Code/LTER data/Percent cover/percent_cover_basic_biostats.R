rm(list = ls())
rm(list = ls())
library(dplyr)
library(lubridate)
library(tidyr)
library(glmmTMB)

#read in wave raw data
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

percent_cover_avgs <- matrix(NA, dim(percent_cover_data)[1], dim(percent_cover_data)[2]-4) #make blank matrix with same number of rows but less columns (minus 2)
colnames(percent_cover_avgs) <- colnames(percent_cover_data)[c(-3,-4,-5,-7)] #assign column names to new matrix minus columns 3, 4, 5, and 7

Year_vector <- unique(percent_cover_data$YEAR) #make unique vector based off of year column
Site_vector <- unique(percent_cover_data$SITE) #make unique vector based off of site column
COARSE_GROUPING_vector <- unique(percent_cover_data$COARSE_GROUPING) #make unique vector based off of coarse grouping

row_new <- 1 #number of rows added

for(i in Site_vector) #for element in site vector
{
  percent_cover_stuff <- percent_cover_data[LTER_data$SITE == i,] #new matrix for everything lining up with site column
  for(j in Year_vector) #for all years
  {
    
    percent_cover_thing <- percent_cover_stuff[percent_cover_stuff$YEAR == j,] #make matrix assigning everything by year
    
    mtrans <- length(unique(percent_cover_thing$TRANSECT)) #number of transects = unique number of transect entries per year
    
    blank <- all(is.na(percent_cover_thing$PERCENT_COVER)) #everything that is na in percent_cover
    
    for(k in COARSE_GROUPING_vector) #for everything in the different functional groups
    {
      percent_cover_avgs[row_new,1] <- j #column 1 assigns to the year with new data entry k
      percent_cover_avgs[row_new,2] <- i #column 2 assigns new row to column entry i
      percent_cover_avgs[row_new,4] <- k #column 4 assigns new row to column entry k
      
      percent_cover_knacks <- percent_cover_thing[percent_cover_thing$COARSE_GROUPING == k,]  #assigns by coarse grouping
      
      out <- (sum(percent_cover_knacks$PERCENT_COVER, na.rm = TRUE))/mtrans #averages things in percent_cover column by number of transects and gets rid of nas--sums everything in quads and divides by number of transects
      
      if(length(out)==0) #if blank
      {
        out <- NA #assign na
        if(blank==FALSE) #if not blank
        {
          out <- 0 #then give it 0
        }
      }
      
      
      percent_cover_avgs[row_new,3] <- out #enter everything into the third column
      
      row_new <- row_new+1 #iterate by adding additional row
    }
  }
}

percent_cover_avgs[-which(is.na(percent_cover_avgs[,3]))] #get rid of all rows which has a percent_cover of nas

percent_cover_avgs <- as.data.frame(percent_cover_avgs) #make into dataframe

percent_cover_avgs2 <- percent_cover_avgs %>%
  pivot_wider(names_from = COARSE_GROUPING, values_from = PERCENT_COVER) #https://stackoverflow.com/questions/64032333/change-column-data-into-column-names
#Draws data from the percent_cover columns and pivots table so that it's all in columns for each functional group
#ask about error

percent_cover_avgs2 <- percent_cover_avgs2[,c("SITE","YEAR","MOBILE INVERT","EPILITHIC SESSILE INVERT","ENDOLITHIC SESSILE INVERT","UNDERSTORY ALGAE","GIANT KELP","SESSILE PLANT")]
#gets rid of null columns
percent_cover_avgs2 <- percent_cover_avgs2[-(length(percent_cover_avgs2$YEAR)),]

#enter in wave data
wave_data <- read.csv("./Wavekelpthesis/Edited_data/mod_wave_subset.csv")
wave_data <- wave_data[,c("site", "Max_Hs_m", "wave_yr")]

#merge wave data and percent_cover by "site" and "year/wave year" columns
percent_cover_comparison_all <- merge(y = percent_cover_avgs2, x = wave_data, by.y = c("SITE","YEAR"), by.x = c("site", "wave_yr"))


#verify reason why percent_cover_comparison_all is 50 obs less is because of channel island data deletions
not_in_merged_df <- percent_cover_avgs2 %>% filter(!SITE %in% percent_cover_comparison_all$site)

#convert to numeric data
percent_cover_comparison_all$wave_yr <- as.numeric(unlist(percent_cover_comparison_all$wave_yr))
percent_cover_comparison_all$Max_Hs_m <- as.numeric(unlist(percent_cover_comparison_all$Max_Hs_m))
percent_cover_comparison_all$`EPILITHIC SESSILE INVERT` <- as.numeric(unlist(percent_cover_comparison_all$`EPILITHIC SESSILE INVERT`))
percent_cover_comparison_all$`UNDERSTORY ALGAE` <- as.numeric(unlist(percent_cover_comparison_all$`UNDERSTORY ALGAE`))
percent_cover_comparison_all$`ENDOLITHIC SESSILE INVERT` <- as.numeric(unlist(percent_cover_comparison_all$`ENDOLITHIC SESSILE INVERT`))
percent_cover_comparison_all$`GIANT KELP` <- as.numeric(unlist(percent_cover_comparison_all$`GIANT KELP`))
percent_cover_comparison_all$`MOBILE INVERT` <- as.numeric(unlist(percent_cover_comparison_all$`MOBILE INVERT`))
percent_cover_comparison_all$`SESSILE PLANT` <- as.numeric(unlist(percent_cover_comparison_all$`SESSILE PLANT`))

#convert all Nans to NAs
for(i in 1:dim(percent_cover_comparison_all)[1]){
  for(j in 4:dim(percent_cover_comparison_all)[2])
  {
    if(is.nan(percent_cover_comparison_all[i,j]) == TRUE)
    {
      percent_cover_comparison_all[i,j] <- NA
    } else {
      next
    }
  }
}

percent_cover_comparison_all <- percent_cover_comparison_all %>% filter(is.na(`EPILITHIC SESSILE INVERT`) %in% percent_cover_comparison_all$`EPILITHIC SESSILE INVERT`)




#subset by functional groups
mobile_invert = percent_cover_comparison_all[,c("site","wave_yr","MOBILE INVERT","Max_Hs_m")]
epi_sessile_invert = percent_cover_comparison_all[,c("site","wave_yr","EPILITHIC SESSILE INVERT","Max_Hs_m")]
endo_sessile_invert = percent_cover_comparison_all[,c("site","wave_yr","ENDOLITHIC SESSILE INVERT","Max_Hs_m")]
understory_algae = percent_cover_comparison_all[,c("site","wave_yr","UNDERSTORY ALGAE","Max_Hs_m")]
giant_kelp = percent_cover_comparison_all[,c("site","wave_yr","GIANT KELP","Max_Hs_m")]
sessile_plant = percent_cover_comparison_all[,c("site","wave_yr","SESSILE PLANT","Max_Hs_m")]


pdf(file = "./Wavekelpthesis/Code/LTER data/Percent cover/10-14-24_percentcover_simplelm.pdf", paper = "us")
#simple linear regressions
lr_func <- function(percent_cover_subset){
  percent_cover_subset_lin_reg <- lm(data= percent_cover_subset,formula = percent_cover_subset[,3]~percent_cover_subset$Max_Hs_m)
percent_cover_subset_lin_reg_plot <- plot(y= percent_cover_subset[,3],x=percent_cover_subset$Max_Hs_m,type="p", xlab = "Max wave height (m)", ylab = "percent_cover per site/year (%)", main =  glue("Max winter wave height vs \n{colnames(percent_cover_subset[3])} percent cover"))
  percent_cover_subset_prt_intercept <- coef(summary(percent_cover_subset_lin_reg))[1,'t value']
  percent_cover_subset_prt_waves <- coef(summary(percent_cover_subset_lin_reg))[2,'t value']
  percent_cover_subset_rsquared <- summary(percent_cover_subset_lin_reg)$r.squared
  percent_cover_subset_fvalue <- summary(percent_cover_subset_lin_reg)$fstatistic[1]
  percent_cover_subset_pvalue <- pf(summary(percent_cover_subset_lin_reg)$fstatistic[1],summary(percent_cover_subset_lin_reg)$fstatistic[2],summary(percent_cover_subset_lin_reg)$fstatistic[3],lower.tail = FALSE)
  abline(percent_cover_subset_lin_reg$coefficients[1],percent_cover_subset_lin_reg$coefficients[2], col = "red")
  text(x = 4.9, y = max(percent_cover_subset[3])-(max(percent_cover_subset[3])*0.2), label = glue("Pr(>|t|) = {round(percent_cover_subset_prt_intercept,digits = 6)} (intercept) \nPr(>|t|) = {round(percent_cover_subset_prt_waves,digits = 6)} (waves) \nr-squared = {round(percent_cover_subset_rsquared,digits = 6)} \nF-value = {round(percent_cover_subset_fvalue,digits = 6)} \np-value = {round(percent_cover_subset_pvalue,digits = 6)}"), cex = 0.5, pos = 2)
  return(percent_cover_subset_lin_reg_plot)
}
lr_func(mobile_invert)
lr_func(epi_sessile_invert)
lr_func(endo_sessile_invert)
lr_func(understory_algae)
lr_func(giant_kelp)
lr_func(sessile_plant)

dev.off()

mobile_invert_rocksite_pc <- merge(x = mobile_invert, y = rock_site_avgs_1, by.y = c("SITE","YEAR"), by.x =  c("site","wave_yr"))
year.factor <- (mobile_invert_rocksite_pc$wave_yr)

mobile_invert_glmmTMB <- glmmTMB(biomass_glmmtmb_transavg$`MOBILE INVERT`~biomass_glmmtmb_transavg$Max_Hs_m + (1|mobile_invert_rocksite_pc$wave_yr) + (1|mobile_invert_rocksite_pc$PERCENT_ROCK_COVER)+ ar1(year.factor+0|mobile_invert_rocksite_pc$site),family = beta)
