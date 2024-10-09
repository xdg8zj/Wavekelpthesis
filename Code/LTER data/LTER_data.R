rm(list = ls())
rm(list = ls())
library(dplyr)
library(lubridate)

#read in wave raw data
setwd("/Users/aleynaloughran-pierce/Desktop")
#reads in LTER Data


LTER_data <- read.csv("./Wavekelpthesis/Raw Files/Annual_All_Species_Biomass_at_transect_20240501.csv")
#summer months range from early july to late october (month = 7: 10)

#sorts out columns in data for what we want
LTER_data <- LTER_data[,c("YEAR", "SITE", "TRANSECT", "AFDM", "SCIENTIFIC_NAME", "COARSE_GROUPING")]


for(animal in 1:length(LTER_data$AFDM))
{
  if(LTER_data$AFDM[animal] == -99999.00000)
  {
    LTER_data$AFDM[animal] = NA
  }
}

for(animal in 1:length(LTER_data$SCIENTIFIC_NAME))
{
  if(LTER_data$SCIENTIFIC_NAME[animal] == "Parapholas californica")
  {
    LTER_data$COARSE_GROUPING[animal] <- "ENDOLITHIC SESSILE INVERT"
  }
}

for(animal in 1:length(LTER_data$SCIENTIFIC_NAME))
{
  if(LTER_data$SCIENTIFIC_NAME[animal] == "Chaceia ovoidea")
  {
    LTER_data$COARSE_GROUPING[animal] <- "ENDOLITHIC SESSILE INVERT"
  }
}

for(animal in 1:length(LTER_data$COARSE_GROUPING))
{
  if(LTER_data$COARSE_GROUPING[animal] == "SESSILE INVERT")
  {
    LTER_data$COARSE_GROUPING[animal] <- "EPILITHIC SESSILE INVERT"
  }
}

LTER_avgs <- matrix(NA, dim(LTER_data)[1], dim(LTER_data)[2]-2) #make blank matrix with same number of rows but less columns (minus 2)
colnames(LTER_avgs) <- colnames(LTER_data)[c(-3,-5)] #assign column names to new matrix minus columns 3 and 5

Year_vector <- unique(LTER_data$YEAR) #make unique vector based off of year column
Site_vector <- unique(LTER_data$SITE) #make unique vector based off of site column
COARSE_GROUPING_vector <- unique(LTER_data$COARSE_GROUPING) #make unique vector based off of coarse grouping

row_new <- 1 #number of rows added

for(i in Site_vector) #for element in site vector
{
  LTER_stuff <- LTER_data[LTER_data$SITE == i,] #new matrix for everything lining up with site column
  for(j in Year_vector) #for all years
  {
    
    LTER_thing <- LTER_stuff[LTER_stuff$YEAR == j,] #make matrix assigning everything by year
    
    mtrans <- length(unique(LTER_thing$TRANSECT)) #number of transects = unique number of transect entries per year
    
    blank <- all(is.na(LTER_thing$AFDM)) #everything that is na in biomass
    
    for(k in COARSE_GROUPING_vector) #for everything in the different functional groups
    {
      LTER_avgs[row_new,1] <- j #column 1 assigns to the year with new data entry k
      LTER_avgs[row_new,2] <- i #column 2 assigns new row to column entry i
      LTER_avgs[row_new,4] <- k #column 4 assigns new row to column entry k
      
      LTER_knacks <- LTER_thing[LTER_thing$COARSE_GROUPING == k,]  #assigns by coarse grouping
      
      out <- (sum(LTER_knacks$AFDM, na.rm = TRUE))/mtrans #averages things in biomass column by number of transects and gets rid of nas
      
      if(length(out)==0) #if blank
      {
        out <- NA #assign na
        if(blank==FALSE) #if not blank
        {
          out <- 0 #then give it 0
        }
      }
      
      
      LTER_avgs[row_new,3] <- out #enter everything into the third column
      
      row_new <- row_new+1 #iterate by adding additional row
    }
  }
}

LTER_avgs[-which(is.na(LTER_avgs[,3]))] #get rid of all rows which has a biomass of nas

LTER_avgs <- as.data.frame(LTER_avgs)



library(dplyr)
library(tidyr)
LTER_avgs2 <- LTER_avgs %>%
  pivot_wider(names_from = COARSE_GROUPING, values_from = AFDM) #https://stackoverflow.com/questions/64032333/change-column-data-into-column-names
#Draws data from the biomass columns and pivots table so that it's all in columns for each functional group
LTER_avgs2 <- LTER_avgs2[,c("YEAR","SITE","EPILITHIC SESSILE INVERT","ENDOLITHIC SESSILE INVERT","MOBILE INVERT","UNDERSTORY ALGAE","GIANT KELP","FISH")]
#gets rid of null columns
LTER_avgs2 <- LTER_avgs2[-265,]

#enter in wave datasets
wave_data <- read.csv("/Users/aleynaloughran-pierce/Desktop/CHMBE_Summer_2024/Edited_data/CDIP_Waveperiods.csv")
wave_data <- wave_data[,c("YEAR", "SITE", "WINTER_MEAN_HS_M","WINTER_MAX_HS_M")]

#merge LTER_avgs2 and wave_data to get a combined data set with common site and year
LTER_comparison_all <- merge(LTER_avgs2,wave_data)
LTER_comparison_all <- as.data.frame(LTER_comparison_all)
LTER_comparison_all <- LTER_comparison_all[,c("SITE","YEAR","MOBILE INVERT","EPILITHIC SESSILE INVERT","ENDOLITHIC SESSILE INVERT","UNDERSTORY ALGAE","GIANT KELP","FISH","WINTER_MEAN_HS_M","WINTER_MAX_HS_M")]

for(rcounter in 2:dim(LTER_comparison_all)[2])
{
  LTER_comparison_all[[rcounter]] <- as.numeric(LTER_comparison_all[[rcounter]])
}


for(i in 1:dim(LTER_comparison_all)[1]){
  for(j in 3:dim(LTER_comparison_all)[2])
  {
    if(is.nan(LTER_comparison_all[i,j]) == TRUE)
    {
      LTER_comparison_all[i,j] <- NA
    } else {
      next
    }
  }
}



#simple linear model
pdf(file = "/Users/aleynaloughran-pierce/Desktop/CHMBE_Summer_2024/Code/LTER data/RESULTS/LTER_data_simple_lm_07_19_2024.pdf", paper = "us")
mobile_invert_lm_max <- lm(data= LTER_comparison_all,formula = LTER_comparison_all$`MOBILE INVERT`~LTER_comparison_all$WINTER_MAX_HS_M)
summary(mobile_invert_lm_max)
summary(mobile_invert_lm_max)$r.squared
plot(y= LTER_comparison_all$`MOBILE INVERT`,x=LTER_comparison_all$WINTER_MAX_HS_M,type="p", xlab = "Max wave height (m)", ylab = "Biomass per site/year (g)", main = "Wave Height vs Biomass (Mobile Inverts)")
abline(mobile_invert_lm_max$coefficients[1],mobile_invert_lm_max$coefficients[2], col = "red")
# legend(x="topleft", legend=unique(as.factor(LTER_comparison_all$YEAR)), pch=16, col=rbPal(24), cex = 0.7)
text(4,150, label= c("Pr(>|t|) (intercept) = 0.00445 \n Pr(>|t|) (waves) = 0.26065 \n F-stat = 1.272 \n r-squared = 0.006079298"), cex = 0.7)
#mean waves:
mobile_invert_lm_mean <- lm(data= LTER_comparison_all,formula = LTER_comparison_all$`MOBILE INVERT`~LTER_comparison_all$WINTER_MEAN_HS_M)
summary(mobile_invert_lm_mean)
summary(mobile_invert_lm_mean)$r.squared
plot(y= LTER_comparison_all$`MOBILE INVERT`,x=LTER_comparison_all$WINTER_MEAN_HS_M,type="p", xlab = "Mean wave height (m)", ylab = "Biomass per site/year (g)", main = "Wave Height vs Biomass (Mobile Inverts)") 
abline(mobile_invert_lm_mean$coefficients[1],mobile_invert_lm_mean$coefficients[2], col = "red")
text(0.8,150, label= c("Pr(>|t|) (intercept) = 0.5915  \n Pr(>|t|) (waves) = 0.01794 \n F-stat =  5.692 \n r-squared = 0.02663789"), cex = 0.7)


#Epilithic sessile invert lms
#epilithic sessile invert max waves:
epilithic_sessile_invert_lm_max <- lm(data= LTER_comparison_all,formula = LTER_comparison_all$`EPILITHIC SESSILE INVERT`~LTER_comparison_all$WINTER_MAX_HS_M)
summary(epilithic_sessile_invert_lm_max)
summary(epilithic_sessile_invert_lm_max)$r.squared
plot(y= LTER_comparison_all$`EPILITHIC SESSILE INVERT`,x=LTER_comparison_all$WINTER_MAX_HS_M,type="p", xlab = "Max wave height (m)", ylab = "Biomass per site/year (g)", main = "Wave Height vs Biomass (Epilithic Sessile Inverts)") 
abline(epilithic_sessile_invert_lm_max$coefficients[1],epilithic_sessile_invert_lm_max$coefficients[2], col = "red")
text(4,60, label= c("Pr(>|t|) (intercept) = 0.00333   \n Pr(>|t|) (waves) =  0.11378 \n F-stat = 2.522 \n r-squared = 0.01198013"), cex = 0.7)

#epilithic sessile inverts mean waves:
epilithic_sessile_invert_lm_mean <- lm(data= LTER_comparison_all,formula = LTER_comparison_all$`EPILITHIC SESSILE INVERT`~LTER_comparison_all$WINTER_MEAN_HS_M)
summary(epilithic_sessile_invert_lm_mean)
summary(epilithic_sessile_invert_lm_mean)$r.squared
plot(y= LTER_comparison_all$`EPILITHIC SESSILE INVERT`,x=LTER_comparison_all$WINTER_MEAN_HS_M,type="p", xlab = "Mean wave height (m)", ylab = "Biomass per site/year (g)", main = "Wave Height vs Biomass (Epilithic Sessile Inverts)") 
abline(epilithic_sessile_invert_lm_mean$coefficients[1],epilithic_sessile_invert_lm_mean$coefficients[2], col = "red")
text(0.8,60, label= c("Pr(>|t|) (intercept) = 0.2239 \n Pr(>|t|) (waves) = 0.04082 \n F-stat = 4.236 \n r-squared = 0.01995931"), cex = 0.7)

#ENDOLITHIC sessile invert lms
#endolithic sessile invert max waves:
endolithic_sessile_invert_lm_max <- lm(data= LTER_comparison_all,formula = LTER_comparison_all$`ENDOLITHIC SESSILE INVERT`~LTER_comparison_all$WINTER_MAX_HS_M)
summary(endolithic_sessile_invert_lm_max)
summary(endolithic_sessile_invert_lm_max)$r.squared
plot(y= LTER_comparison_all$`ENDOLITHIC SESSILE INVERT`,x=LTER_comparison_all$WINTER_MAX_HS_M,type="p", xlab = "Max wave height (m)", ylab = "Biomass per site/year (g)", main = "Wave Height vs Biomass (Endolithic Sessile Inverts)") 
abline(endolithic_sessile_invert_lm_max$coefficients[1],endolithic_sessile_invert_lm_max$coefficients[2], col = "red")
text(4,900, label= c("Pr(>|t|) (intercept) = 0.000133  \n Pr(>|t|) (waves) =  0.513845 \n F-stat = 0.4277 \n r-squared = 0.002052003"), cex = 0.7)

#endolithic sessile inverts mean waves:
endolithic_sessile_invert_lm_mean <- lm(data= LTER_comparison_all,formula = LTER_comparison_all$`ENDOLITHIC SESSILE INVERT`~LTER_comparison_all$WINTER_MEAN_HS_M)
summary(endolithic_sessile_invert_lm_mean)
summary(endolithic_sessile_invert_lm_mean)$r.squared
plot(y= LTER_comparison_all$`ENDOLITHIC SESSILE INVERT`,x=LTER_comparison_all$WINTER_MEAN_HS_M,type="p", xlab = "Mean wave height (m)", ylab = "Biomass per site/year (g)", main = "Wave Height vs Biomass (Endolithic Sessile Inverts)") 
abline(endolithic_sessile_invert_lm_mean$coefficients[1],endolithic_sessile_invert_lm_mean$coefficients[2], col = "red")
text(0.8,900, label= c("Pr(>|t|) (intercept) = 6.55e-07 \n Pr(>|t|) (waves) = 0.005317 \n F-stat = 7.935 \n r-squared = 0.03674512"), cex = 0.7)


#Understory Algae
#max waves
understory_algae_lm <- lm(data= LTER_comparison_all,formula = LTER_comparison_all$`UNDERSTORY ALGAE`~LTER_comparison_all$WINTER_MAX_HS_M)
summary(understory_algae_lm)
summary(understory_algae_lm)$r.squared
plot(y= LTER_comparison_all$`UNDERSTORY ALGAE`,x=LTER_comparison_all$WINTER_MAX_HS_M,type="p", xlab = "Max wave height (m)", ylab = "Biomass per site/year (g)", main = "Wave Height vs Biomass (Understory Algae)") 
abline(understory_algae_lm$coefficients[1],understory_algae_lm$coefficients[2], col = "red")
text(4,150, label= c("Pr(>|t|) (intercept) = 2.74e-07 \n Pr(>|t|) (waves) = 0.03779 \n F-stat = 4.37 \n r-squared =  0.02057843"), cex = 0.7)
#mean waves
understory_algae_lm_mean <- lm(data= LTER_comparison_all,formula = LTER_comparison_all$`UNDERSTORY ALGAE`~LTER_comparison_all$WINTER_MEAN_HS_M)
summary(understory_algae_lm_mean)
plot(y= LTER_comparison_all$`UNDERSTORY ALGAE`,x=LTER_comparison_all$WINTER_MEAN_HS_M,type="p", xlab = "Mean wave height (m)", ylab = "Biomass per site/year (g)", main = "Wave Height vs Biomass (Understory Algae)")
abline(understory_algae_lm_mean$coefficients[1],understory_algae_lm_mean$coefficients[2], col = "red")
text(0.8,150, label= c("Pr(>|t|) (intercept) = 0.00218 \n Pr(>|t|) (waves) = 0.02927 \n F-stat = 4.818 \n r-squared = 0.02263861"), cex = 0.7)


#Giant kelp
#max waves
giant_kelp_lm <- lm(data= LTER_comparison_all,formula = LTER_comparison_all$`GIANT KELP`~LTER_comparison_all$WINTER_MAX_HS_M)
summary(giant_kelp_lm)
plot(y= LTER_comparison_all$`GIANT KELP`,x=LTER_comparison_all$WINTER_MAX_HS_M,type="p", xlab = "Max wave height (m)", ylab = "Biomass per site/year (g)", main = "Wave Height vs Biomass (Giant Kelp)")
abline(giant_kelp_lm$coefficients[1],giant_kelp_lm$coefficients[2], col = "red")
text(4,300, label= c("Pr(>|t|) (intercept) = 4.88e-08 \n Pr(>|t|) (waves) =  0.662 \n F-stat = 0.1917 \n r-squared = 0.0009207581"), cex = 0.7)

#mean waves
giant_kelp_lm_mean <- lm(data= LTER_comparison_all,formula = LTER_comparison_all$`GIANT KELP`~LTER_comparison_all$WINTER_MEAN_HS_M)
summary(giant_kelp_lm_mean)
plot(y= LTER_comparison_all$`GIANT KELP`,x=LTER_comparison_all$WINTER_MEAN_HS_M,type="p", xlab = "Mean wave height (m)", ylab = "Biomass per site/year (g)", main = "Wave Height vs Biomass (GIANT KELP)") 
abline(giant_kelp_lm_mean$coefficients[1],giant_kelp_lm_mean$coefficients[2], col = "red")
text(0.8,300, label= c("Pr(>|t|) (intercept) = 0.000116 \n Pr(>|t|) (waves) =  0.865796 \n F-stat = 0.02863 \n r-squared = 0.0001376355"), cex = 0.7)

fish_lm_max <- lm(data= LTER_comparison_all,formula = LTER_comparison_all$FISH~LTER_comparison_all$WINTER_MAX_HS_M)
summary(fish_lm_max)
summary(fish_lm_max)$r.squared
plot(y= LTER_comparison_all$FISH,x=LTER_comparison_all$WINTER_MAX_HS_M,type="p", xlab = "Max wave height (m)", ylab = "Biomass per site/year (g)", main = "Wave Height vs Biomass (Fish)")
abline(fish_lm_max$coefficients[1],fish_lm_max$coefficients[2], col = "red")
text(4,50, label= c("Pr(>|t|) (intercept) = 0.00489 \n Pr(>|t|) (waves) = 0.74626 \n F-stat = 0.105 \n r-squared = 0.0005044398"), cex = 0.7)


#mean waves
fish_lm_mean <- lm(data= LTER_comparison_all,formula = LTER_comparison_all$`FISH`~LTER_comparison_all$WINTER_MEAN_HS_M)
summary(fish_lm_mean)
plot(y= LTER_comparison_all$`FISH`,x=LTER_comparison_all$WINTER_MEAN_HS_M,type="p", xlab = "Mean wave height (m)", ylab = "Biomass per site/year (g)", main = "Wave Height vs Biomass (fish)")
abline(fish_lm_mean$coefficients[1],fish_lm_mean$coefficients[2], col = "red")
text(0.8,60, label= c("Pr(>|t|) (intercept) = 0.0779 \n Pr(>|t|) (waves) = 0.6152 \n F-stat = 0.2534 \n r-squared =  0.001216655"), cex = 0.7)



dev.off()



pdf(file = "/Users/aleynaloughran-pierce/Desktop/CHMBE_Summer_2024/Code/LTER data/RESULTS/LTER_log_plus_1_lm_07_19_24.pdf", paper = "us")


log_transformed_biomass <- LTER_comparison_all
log_transformed_biomass$`UNDERSTORY ALGAE` <- log(log_transformed_biomass$`UNDERSTORY ALGAE`+1)
log_transformed_biomass$`GIANT KELP` <- log(log_transformed_biomass$`GIANT KELP`+1)
log_transformed_biomass$`MOBILE INVERT` <- log(log_transformed_biomass$`MOBILE INVERT`+1)
log_transformed_biomass$`EPILITHIC SESSILE INVERT` <- log(log_transformed_biomass$`EPILITHIC SESSILE INVERT`+1)
log_transformed_biomass$`ENDOLITHIC SESSILE INVERT` <- log(log_transformed_biomass$`ENDOLITHIC SESSILE INVERT`+1)
log_transformed_biomass$FISH <- log(log_transformed_biomass$FISH+1)
#LINEAR MODELS
#MOBILE INVERTS
#Max waves
log_mobile_invert_lm_max <- lm(data= log_transformed_biomass,formula = log_transformed_biomass$`MOBILE INVERT`~log_transformed_biomass$WINTER_MAX_HS_M)
summary(log_mobile_invert_lm_max)
plot(y= log_transformed_biomass$`MOBILE INVERT`,x=log_transformed_biomass$WINTER_MAX_HS_M,type="p", xlab = "Max wave height (m)", ylab = "[log+1] Biomass per site/year (g)", main = "Wave Height vs Biomass (Mobile Inverts)")
abline(log_mobile_invert_lm_max$coefficients[1],log_mobile_invert_lm_max$coefficients[2], col = "red")
text(4,0.8, label= c("Pr(>|t|) (intercept) = 1.93e-13 \n Pr(>|t|) (waves) = 0.1401 \n F-stat = 2.193 \n r-squared = 0.01043396"), cex = 0.7)
#mean waves:
log_mobile_invert_lm_mean <- lm(data= log_transformed_biomass,formula = log_transformed_biomass$`MOBILE INVERT`~log_transformed_biomass$WINTER_MEAN_HS_M)
summary(log_mobile_invert_lm_mean)
plot(y= log_transformed_biomass$`MOBILE INVERT`,x=log_transformed_biomass$WINTER_MEAN_HS_M,type="p", xlab = "Mean wave height (m)", ylab = "[log+1] Biomass per site/year (g)", main = "Wave Height vs Biomass (Mobile Inverts)") 
abline(log_mobile_invert_lm_mean$coefficients[1],log_mobile_invert_lm_mean$coefficients[2], col = "red")
text(0.8,1.5, label= c("Pr(>|t|) (intercept) = 0.00025  \n Pr(>|t|) (waves) = 0.002011 \n F-stat = 9.786 \n r-squared = 0.04493278"), cex = 0.7)


#Epilithic Sessile invert lms
#epilithic Sessile invert max waves:
log_epi_sessile_invert_lm_max <- lm(data= log_transformed_biomass,formula = log_transformed_biomass$`EPILITHIC SESSILE INVERT`~log_transformed_biomass$WINTER_MAX_HS_M)
summary(log_epi_sessile_invert_lm_max)
summary(log_epi_sessile_invert_lm_max)$r.squared
plot(y= log_transformed_biomass$`EPILITHIC SESSILE INVERT`,x=log_transformed_biomass$WINTER_MAX_HS_M,type="p", xlab = "Max wave height (m)", ylab = "[log+1] Biomass per site/year (g)", main = "Wave Height vs Biomass (Epilithic Sessile Inverts)") 
abline(log_epi_sessile_invert_lm_max$coefficients[1],log_epi_sessile_invert_lm_max$coefficients[2], col = "red")
text(4,1, label= c("Pr(>|t|) (intercept) = 8.51e-15 \n Pr(>|t|) (waves) =  0.03882 \n F-stat = 4.323 \n r-squared = 0.020362125"), cex = 0.7)
#sessile inverts mean waves:
log_epi_sessile_invert_lm_mean <- lm(data= log_transformed_biomass,formula = log_transformed_biomass$`EPILITHIC SESSILE INVERT`~log_transformed_biomass$WINTER_MEAN_HS_M)
summary(log_epi_sessile_invert_lm_mean)
plot(y= log_transformed_biomass$`EPILITHIC SESSILE INVERT`,x=log_transformed_biomass$WINTER_MEAN_HS_M,type="p", xlab = "Mean wave height (m)", ylab = "[log+1] Biomass per site/year (g)", main = "Wave Height vs Biomass (Epilithic Sessile Inverts)") 
abline(log_epi_sessile_invert_lm_mean$coefficients[1],log_epi_sessile_invert_lm_mean$coefficients[2], col = "red")
text(0.8,1, label= c("Pr(>|t|) (intercept) = 7.79e-05 \n Pr(>|t|) (waves) = 0.0003481 \n F-stat = 13.23 \n r-squared = 0.05978582"), cex = 0.7)

#Endolithic Sessile invert lms
#Endolithic Sessile invert max waves:
log_endo_sessile_invert_lm_max <- lm(data= log_transformed_biomass,formula = log_transformed_biomass$`ENDOLITHIC SESSILE INVERT`~log_transformed_biomass$WINTER_MAX_HS_M)
summary(log_endo_sessile_invert_lm_max)
summary(log_endo_sessile_invert_lm_max)$r.squared
plot(y= log_transformed_biomass$`ENDOLITHIC SESSILE INVERT`,x=log_transformed_biomass$WINTER_MAX_HS_M,type="p", xlab = "Max wave height (m)", ylab = "[log+1] Biomass per site/year (g)", main = "Wave Height vs Biomass (Endolithic Sessile Inverts)") 
abline(log_endo_sessile_invert_lm_max$coefficients[1],log_endo_sessile_invert_lm_max$coefficients[2], col = "red")
text(4,4, label= c("Pr(>|t|) (intercept) = 3.89e-12 \n Pr(>|t|) (waves) =  0.9871 \n F-stat = 0.0002613 \n r-squared = 1.25611e-06"), cex = 0.7)
#endolithic sessile inverts mean waves:
log_endo_sessile_invert_lm_mean <- lm(data= log_transformed_biomass,formula = log_transformed_biomass$`ENDOLITHIC SESSILE INVERT`~log_transformed_biomass$WINTER_MEAN_HS_M)
summary(log_endo_sessile_invert_lm_mean)
plot(y= log_transformed_biomass$`ENDOLITHIC SESSILE INVERT`,x=log_transformed_biomass$WINTER_MEAN_HS_M,type="p", xlab = "Mean wave height (m)", ylab = "[log+1] Biomass per site/year (g)", main = "Wave Height vs Biomass (Endolithic Sessile Inverts)") 
abline(log_endo_sessile_invert_lm_mean$coefficients[1],log_endo_sessile_invert_lm_mean$coefficients[2], col = "red")
text(0.8,0.75, label= c("Pr(>|t|) (intercept) = 6.96e-11 \n Pr(>|t|) (waves) = 0.1089 \n F-stat = 2.592 \n r-squared = 0.0123092"), cex = 0.7)

#Understory Algae
#max waves
log_understory_algae_lm <- lm(data= log_transformed_biomass,formula = log_transformed_biomass$`UNDERSTORY ALGAE`~log_transformed_biomass$WINTER_MAX_HS_M)
summary(log_understory_algae_lm)
summary(log_understory_algae_lm)$r.squared
plot(y= log_transformed_biomass$`UNDERSTORY ALGAE`,x=log_transformed_biomass$WINTER_MAX_HS_M,type="p", xlab = "Max wave height (m)", ylab = "[log+1] Biomass per site/year (g)", main = "Wave Height vs Biomass (Understory Algae)") 
abline(log_understory_algae_lm$coefficients[1],log_understory_algae_lm$coefficients[2], col = "red")
text(4,2, label= c("Pr(>|t|) (intercept) = <2e-16 \n Pr(>|t|) (waves) = 0.1075 \n F-stat = 2.612 \n r-squared =  0.01240409"), cex = 0.7)
#mean waves
log_understory_algae_lm_mean <- lm(data= log_transformed_biomass,formula = log_transformed_biomass$`UNDERSTORY ALGAE`~log_transformed_biomass$WINTER_MEAN_HS_M)
summary(log_understory_algae_lm_mean)
plot(y= log_transformed_biomass$`UNDERSTORY ALGAE`,x=log_transformed_biomass$WINTER_MEAN_HS_M,type="p", xlab = "Mean wave height (m)", ylab = "[log+1] Biomass per site/year (g)", main = "Wave Height vs Biomass (Understory Algae)")
abline(log_understory_algae_lm_mean$coefficients[1],log_understory_algae_lm_mean$coefficients[2], col = "red")
text(0.8,2, label= c("Pr(>|t|) (intercept) = <2e-16 \n Pr(>|t|) (waves) = 0.04847 \n F-stat = 3.94 \n r-squared = 0.01858916"), cex = 0.7)


#Giant kelp
#max waves
log_giant_kelp_lm <- lm(data= log_transformed_biomass,formula = log_transformed_biomass$`GIANT KELP`~log_transformed_biomass$WINTER_MAX_HS_M)
summary(log_giant_kelp_lm)
plot(y= log_transformed_biomass$`GIANT KELP`,x=log_transformed_biomass$WINTER_MAX_HS_M,type="p", xlab = "Max wave height (m)", ylab = "[log+1] Biomass per site/year (g)", main = "Wave Height vs Biomass (Giant Kelp)")
abline(log_giant_kelp_lm$coefficients[1],log_giant_kelp_lm$coefficients[2], col = "red")
text(4,2, label= c("Pr(>|t|) (intercept) = <2e-16 \n Pr(>|t|) (waves) =  0.376 \n F-stat = 0.7871 \n r-squared = 0.003769892"), cex = 0.7)

#mean waves
log_giant_kelp_lm_mean <- lm(data= log_transformed_biomass,formula = log_transformed_biomass$`GIANT KELP`~log_transformed_biomass$WINTER_MEAN_HS_M)
summary(log_giant_kelp_lm_mean)
plot(y= log_transformed_biomass$`GIANT KELP`,x=log_transformed_biomass$WINTER_MEAN_HS_M,type="p", xlab = "Mean wave height (m)", ylab = "[log+1] Biomass per site/year (g)", main = "Wave Height vs Biomass (Giant Kelp)") 
abline(log_giant_kelp_lm_mean$coefficients[1],log_giant_kelp_lm_mean$coefficients[2], col = "red")
text(0.35,1, label= c("Pr(>|t|) (intercept) = <2e-16 \n Pr(>|t|) (waves) =  0.6649 \n F-stat = 0.1881 \n r-squared = 0.0009036908"), cex = 0.7)

log_fish_lm_max <- lm(data= log_transformed_biomass,formula = log_transformed_biomass$FISH~log_transformed_biomass$WINTER_MAX_HS_M)
summary(log_fish_lm_max)
summary(log_fish_lm_max)$r.squared
plot(y= log_transformed_biomass$FISH,x=log_transformed_biomass$WINTER_MAX_HS_M,type="p", xlab = "Max wave height (m)", ylab = "[log+1] Biomass per site/year (g)", main = "Wave Height vs Biomass (Fish)")
abline(log_fish_lm_max$coefficients[1],log_fish_lm_max$coefficients[2], col = "red")
text(4,3.5, label= c("Pr(>|t|) (intercept) = 3.39e-09 \n Pr(>|t|) (waves) = 0.08532 \n F-stat = 2.989 \n r-squared = 0.01416632"), cex = 0.7)


#mean waves
log_fish_lm_mean <- lm(data= log_transformed_biomass,formula = log_transformed_biomass$`FISH`~log_transformed_biomass$WINTER_MEAN_HS_M)
summary(log_fish_lm_mean)
plot(y= log_transformed_biomass$`FISH`,x=log_transformed_biomass$WINTER_MEAN_HS_M,type="p", xlab = "Mean wave height (m)", ylab = "[log+1] Biomass per site/year (g)", main = "Wave Height vs Biomass (Fish)") 
abline(log_fish_lm_mean$coefficients[1],log_fish_lm_mean$coefficients[2], col = "red")
text(0.8,3.6, label= c("Pr(>|t|) (intercept) = 0.000836 \n Pr(>|t|) (waves) = 0.021934 \n F-stat = 5.331 \n r-squared =  0.02498848"), cex = 0.7)

#summary:
#mobile sessile inverts: intercept significant with Pr(>|t|) = 0.00445
dev.off()
# fix with for-loops later


#correlation test (spearman)

cor.test(y = LTER_comparison_all$`MOBILE INVERT`, x = LTER_comparison_all$WINTER_MAX_HS_M, method = "spearman", exact = FALSE)
cor.test(y = LTER_comparison_all$`MOBILE INVERT`, x = LTER_comparison_all$WINTER_MEAN_HS_M, method = "spearman", exact = FALSE)
cor.test(y = LTER_comparison_all$`EPILITHIC SESSILE INVERT`, x = LTER_comparison_all$WINTER_MAX_HS_M, method = "spearman", exact = FALSE)
cor.test(y = LTER_comparison_all$`EPILITHIC SESSILE INVERT`, x = LTER_comparison_all$WINTER_MEAN_HS_M, method = "spearman", exact = FALSE)
cor.test(y = LTER_comparison_all$`ENDOLITHIC SESSILE INVERT`, x = LTER_comparison_all$WINTER_MAX_HS_M, method = "spearman", exact = FALSE)
cor.test(y = LTER_comparison_all$`ENDOLITHIC SESSILE INVERT`, x = LTER_comparison_all$WINTER_MEAN_HS_M, method = "spearman", exact = FALSE)
cor.test(y = LTER_comparison_all$`UNDERSTORY ALGAE`, x = LTER_comparison_all$WINTER_MAX_HS_M, method = "spearman", exact = FALSE)
cor.test(y = LTER_comparison_all$`UNDERSTORY ALGAE`, x = LTER_comparison_all$WINTER_MEAN_HS_M, method = "spearman", exact = FALSE)
cor.test(y = LTER_comparison_all$`GIANT KELP`, x = LTER_comparison_all$WINTER_MAX_HS_M, method = "spearman", exact = FALSE)
cor.test(y = LTER_comparison_all$`GIANT KELP`, x = LTER_comparison_all$WINTER_MEAN_HS_M, method = "spearman", exact = FALSE)
cor.test(y = LTER_comparison_all$`FISH`, x = LTER_comparison_all$WINTER_MAX_HS_M, method = "spearman", exact = FALSE)
cor.test(y = LTER_comparison_all$`FISH`, x = LTER_comparison_all$WINTER_MEAN_HS_M, method = "spearman", exact = FALSE)


#fish site plots

pdf(file = "/Users/aleynaloughran-pierce/Desktop/CHMBE_Summer_2024/Code/LTER data/RESULTS/Fish_site_year_lms_07_19_24.pdf", paper = "us")
require(pals)
site_subset <- na.omit(LTER_comparison_all)
plot(data = site_subset, x = site_subset$WINTER_MAX_HS_M, y = site_subset$FISH, col = unique(as.factor(site_subset$SITE)), xlab = "Max wave height (m)", ylab = "Biomass per site/year (g)", main = "Wave Height vs Biomass (Fish)")
legend("topright", legend = unique(as.factor(site_subset$SITE)), pch = 15, col = unique(as.factor(site_subset$SITE)), cex = 0.7)
abline(fish_lm_max$coefficients[1],fish_lm_max$coefficients[2], col = "black")
text(3.5,55, label= c("Pr(>|t|) (intercept) = 0.00489 \n Pr(>|t|) (waves) = 0.74626 \n F-stat = 0.105 \n r-squared = 0.0005044398"), cex = 0.7)

plot(data = site_subset, x = site_subset$WINTER_MEAN_HS_M, y = site_subset$FISH, col = unique(as.factor(site_subset$SITE)), xlab = "Mean wave height (m)", ylab = "Biomass per site/year (g)", main = "Wave Height vs Biomass (Fish)")
legend("topright", legend = unique(as.factor(site_subset$SITE)), pch = 15, col = unique(as.factor(site_subset$SITE)), cex = 0.7)
abline(fish_lm_mean$coefficients[1],fish_lm_mean$coefficients[2], col = "black")
text(0.7,55, label= c("Pr(>|t|) (intercept) = 0.0779 \n Pr(>|t|) (waves) = 0.6152 \n F-stat = 0.2534 \n r-squared =  0.001216655"), cex = 0.7)

log_site_subset <- site_subset
log_site_subset$`MOBILE INVERT` <- log(log_site_subset$`MOBILE INVERT`+1)
log_site_subset$`EPILITHIC SESSILE INVERT` <- log(log_site_subset$`EPILITHIC SESSILE INVERT`+1)
log_site_subset$`ENDOLITHIC SESSILE INVERT` <- log(log_site_subset$`ENDOLITHIC SESSILE INVERT`+1)
log_site_subset$`UNDERSTORY ALGAE` <- log(log_site_subset$`UNDERSTORY ALGAE`+1)
log_site_subset$`GIANT KELP` <- log(log_site_subset$`GIANT KELP`+1)
log_site_subset$`FISH` <- log(log_site_subset$`FISH`+1)
plot(data = log_site_subset, x = log_site_subset$WINTER_MAX_HS_M, y = log_site_subset$FISH, col = unique(as.factor(log_site_subset$SITE)), xlab = "Max wave height (m)", ylab = "[log+1] Biomass per site/year (g)", main = "Wave Height vs Biomass (Fish)")
legend("topright", legend = unique(as.factor(log_site_subset$SITE)), pch = 15, col = unique(as.factor(log_site_subset$SITE)), cex = 0.7)
abline(log_fish_lm_max$coefficients[1],log_fish_lm_max$coefficients[2], col = "black")
text(3.75,3.5, label= c("Pr(>|t|) (intercept) = 3.39e-09 \n Pr(>|t|) (waves) = 0.08532 \n F-stat = 2.989 \n r-squared = 0.01416632"), cex = 0.7)

plot(data = log_site_subset, x = log_site_subset$WINTER_MEAN_HS_M, y = log_site_subset$FISH, col = unique(as.factor(log_site_subset$SITE)), xlab = "Mean wave height (m)", ylab = "[log+1] Biomass per site/year (g)", main = "Wave Height vs Biomass (Fish)")
legend("bottomright", legend = unique(as.factor(log_site_subset$SITE)), pch = 15, col = unique(as.factor(log_site_subset$SITE)), cex = 0.7)
abline(log_fish_lm_mean$coefficients[1],log_fish_lm_mean$coefficients[2], col = "black")
text(0.75,3.6, label= c("Pr(>|t|) (intercept) = 0.000836 \n Pr(>|t|) (waves) = 0.021934 \n F-stat = 5.331 \n r-squared =  0.02498848"), cex = 0.6)

rbPal <- colorRampPalette(c('red','blue'))
site_subset$Colyr <- rbPal(24)[as.numeric(cut(site_subset$YEAR, breaks = 24))]

#fish year plots (maybe?)
require(pals)
plot(data = site_subset, x = site_subset$WINTER_MAX_HS_M, y = site_subset$FISH, col = site_subset$Col, xlab = "Max wave height (m)", ylab = "Biomass per site/year (g)", main = "Wave Height vs Biomass (Fish)")
legend("topright", legend = unique(as.factor(site_subset$YEAR)), pch = 15, col = unique(site_subset$Colyr), cex = 0.4)
abline(fish_lm_max$coefficients[1],fish_lm_max$coefficients[2], col = "black")
text(3.5,55, label= c("Pr(>|t|) (intercept) = 0.00489 \n Pr(>|t|) (waves) = 0.74626 \n F-stat = 0.105 \n r-squared = 0.0005044398"), cex = 0.7)

require(pals)
plot(data = site_subset, x = site_subset$WINTER_MEAN_HS_M, y = site_subset$FISH, col = site_subset$Col, xlab = "Mean wave height (m)", ylab = "Biomass per site/year (g)", main = "Wave Height vs Biomass (Fish)")
legend("topright", legend = unique(as.factor(site_subset$YEAR)), pch = 15, col = unique(site_subset$Colyr), cex = 0.4)
abline(fish_lm_mean$coefficients[1],fish_lm_mean$coefficients[2], col = "black")
text(0.7,55, label= c("Pr(>|t|) (intercept) = 0.0779 \n Pr(>|t|) (waves) = 0.6152 \n F-stat = 0.2534 \n r-squared =  0.001216655"), cex = 0.7)



log_site_subset <- site_subset
log_site_subset$`MOBILE INVERT` <- log(log_site_subset$`MOBILE INVERT`+1)
log_site_subset$`EPILITHIC SESSILE INVERT` <- log(log_site_subset$`EPILITHIC SESSILE INVERT`+1)
log_site_subset$`ENDOLITHIC SESSILE INVERT` <- log(log_site_subset$`ENDOLITHIC SESSILE INVERT`+1)
log_site_subset$`UNDERSTORY ALGAE` <- log(log_site_subset$`UNDERSTORY ALGAE`+1)
log_site_subset$`GIANT KELP` <- log(log_site_subset$`GIANT KELP`+1)
log_site_subset$`FISH` <- log(log_site_subset$`FISH`+1)

plot(data = log_site_subset, x = log_site_subset$WINTER_MAX_HS_M, y = log_site_subset$FISH, col = log_site_subset$Col, xlab = "Max wave height (m)", ylab = "[log+1] Biomass per site/year (g)", main = "Wave Height vs Biomass (Fish)")
legend("topright", legend = unique(as.factor(log_site_subset$YEAR)), pch = 15, col = unique(log_site_subset$Colyr), cex = 0.3)
abline(log_fish_lm_max$coefficients[1],log_fish_lm_max$coefficients[2], col = "black")
text(3.75,3.5, label= c("Pr(>|t|) (intercept) = 3.39e-09 \n Pr(>|t|) (waves) = 0.08532 \n F-stat = 2.989 \n r-squared = 0.01416632"), cex = 0.7)

plot(data = log_site_subset, x = log_site_subset$WINTER_MEAN_HS_M, y = log_site_subset$FISH, col = log_site_subset$Col, xlab = "Mean wave height (m)", ylab = "[log+1] Biomass per site/year (g)", main = "Wave Height vs Biomass (Fish)")
legend("topright", legend = unique(as.factor(log_site_subset$YEAR)), pch = 15, col = unique(log_site_subset$Colyr), cex = 0.3)
abline(log_fish_lm_mean$coefficients[1],log_fish_lm_mean$coefficients[2], col = "black")
text(0.75,3.6, label= c("Pr(>|t|) (intercept) = 0.000836 \n Pr(>|t|) (waves) = 0.021934 \n F-stat = 5.331 \n r-squared =  0.02498848"), cex = 0.6)

dev.off()


#mixed effects model
library(lme4)

mobile_invert_mem_max <- lmer(site_subset$`MOBILE INVERT`~site_subset$WINTER_MAX_HS_M + (1|site_subset$SITE)+(1|site_subset$YEAR), data = site_subset)
summary(mobile_invert_mem_max)
