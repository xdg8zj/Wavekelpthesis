rm(list = ls())
rm(list = ls())
library(dplyr)
library(lubridate)

#read in wave raw data
setwd("/Users/aleynaloughran-pierce/Desktop")
LTE_summer_data <- read.csv("./Waveskelpthesis/Edited_data/LTE_removal_summer_nojune.csv")

LTE_summer_data$COARSE_GROUPING <- paste(LTE_summer_data$MOBILITY,LTE_summer_data$Unnamed..22)
LTE_summer_data <-LTE_summer_data[LTE_summer_data$TREATMENT != "CONTINUAL",]


# for(i in 1:length(LTE_data$TREATMENT))
# {
#   LTE_data$TREATMENT[i] <- []
#   
# }
LTE_summer_subset<- LTE_summer_data[,c("YEAR", "MONTH", "datetime", "SITE", "TRANSECT", "TREATMENT", "AFDM", "SCIENTIFIC_NAME", "COARSE_GROUPING")]

for(i in 1:length(LTE_summer_subset$SCIENTIFIC_NAME))
{
  if(LTE_summer_subset$SCIENTIFIC_NAME[i] == "Macrocystis pyrifera")
  {
    LTE_summer_subset$COARSE_GROUPING[i] <- "GIANT KELP"
  }
}
  
for(i in 1:length(LTE_summer_subset$COARSE_GROUPING))
{
  if (LTE_summer_subset$COARSE_GROUPING[i] == "SESSILE ALGAE")
  {
    LTE_summer_subset$COARSE_GROUPING[i] <- "UNDERSTORY ALGAE"
  }
}

for(i in 1:length(LTE_summer_subset$COARSE_GROUPING))
{
  if (LTE_summer_subset$COARSE_GROUPING[i] == "MOBILE FISH")
  {
    LTE_summer_subset$COARSE_GROUPING[i] <- "FISH"
  }
}

for(animal in 1:length(LTE_summer_subset$SCIENTIFIC_NAME))
{
  if(LTE_summer_subset$SCIENTIFIC_NAME[animal] == "Parapholas californica")
  {
    LTE_summer_subset$COARSE_GROUPING[animal] <- "ENDOLITHIC SESSILE INVERT"
  }
}

for(animal in 1:length(LTE_summer_subset$SCIENTIFIC_NAME))
{
  if(LTE_summer_subset$SCIENTIFIC_NAME[animal] == "Chaceia ovoidea")
  {
    LTE_summer_subset$COARSE_GROUPING[animal] <- "ENDOLITHIC SESSILE INVERT"
  }
}

for(animal in 1:length(LTE_summer_subset$COARSE_GROUPING))
{
  if(LTE_summer_subset$COARSE_GROUPING[animal] == "SESSILE INVERT")
  {
    LTE_summer_subset$COARSE_GROUPING[animal] <- "EPILITHIC SESSILE INVERT"
  }
}


# LTE_summer_subset <- LTE_summer_subset[,c("YEAR", "SITE", "TREATMENT", "TRANSECT", "AFDM", "SCIENTIFIC_NAME", "COARSE_GROUPING")]

for(animal in 1:length(LTE_summer_subset$AFDM))
{
  if(LTE_summer_subset$AFDM[animal] == -99999.00000)
  {
    LTE_summer_subset$AFDM[animal] = NA
  }
}

LTE_summer_subset[,'YEARS_SINCE_EST'] = NA
for(animal in 1:length(LTE_summer_subset$`YEARS_SINCE_EST`))
{
  LTE_summer_subset$`YEARS_SINCE_EST`[animal] <- LTE_summer_subset$`YEAR`[animal] - 2008
}


# for(i in 1:length(LTE_summer_subset$COARSE_GROUPING))
# {
#   LTE_summer_subset$COARSE_GROUPING[i] <- paste(LTE_summer_subset$TREATMENT[i],LTE_summer_subset$COARSE_GROUPING[i], sep = " ")
# }

LTE_summer_subset <- LTE_summer_subset[,c("SITE","YEAR", "MONTH", "TRANSECT", "TREATMENT", "AFDM", "SCIENTIFIC_NAME", "COARSE_GROUPING", "YEARS_SINCE_EST")]

LTE_summer_subset <- as.data.frame(LTE_summer_subset)

#Getting rid of everything after last annual removal date:
#Sites ending @beginning of 2016: IVEE, NAPL
#Sites ending @beginning of 2017: AQUE, CARP, MOHK
sites_2016 <- c("IVEE","NAPL")
sites_2017 <- c("AQUE","CARP","MOHK")
#for IVEE and NAPL make sure to go back and exclude 2016

LTE_summer_subset <- LTE_summer_subset[LTE_summer_subset$YEAR == c(2008,2009,2010,2011,2012,2013,2014,2015,2016),]

LTE_IVEE <- LTE_summer_subset[LTE_summer_subset$SITE == "IVEE",]
LTE_IVEE <- LTE_IVEE[LTE_IVEE$YEAR < 2016,]

LTE_NAPL <- LTE_summer_subset[LTE_summer_subset$SITE == "NAPL",]
LTE_NAPL <- LTE_NAPL[LTE_NAPL$YEAR < 2016,]


LTE_CARP <- LTE_summer_subset[LTE_summer_subset$SITE == "CARP",]
# LTE_CARP <- LTE_CARP[LTE_CARP$YEAR <= 2017,]
LTE_CARP_mobile_invert <- LTE_CARP[LTE_CARP$COARSE_GROUPING == "MOBILE INVERT",]
LTE_CARP_epi_sessile_invert <- LTE_CARP[LTE_CARP$COARSE_GROUPING == "EPILITHIC SESSILE INVERT",]
LTE_CARP_endo_sessile_invert <- LTE_CARP[LTE_CARP$COARSE_GROUPING == "ENDOLITHIC SESSILE INVERT",]
LTE_CARP_understroy_algae <- LTE_CARP[LTE_CARP$COARSE_GROUPING == "UNDERSTORY ALGAE",]
LTE_CARP_giant_kelp <- LTE_CARP[LTE_CARP$COARSE_GROUPING == "GIANT KELP",]
LTE_CARP_fish <- LTE_CARP[LTE_CARP$COARSE_GROUPING == "FISH",]

CARP_dis_mobile_invert <- data.frame(SITE = character(), YEAR = numeric(), MONTH = numeric(), DISM = numeric(), YEARS_SINCE_EST = numeric(), COARSE_GROUPING = character())

months_vector <- c(7,8,9,10)
years_vector <- c(2008,2009,2010,2011,2012,2013,2014,2015,2016)


CARP_2016_data <- LTE_CARP_mobile_invert[LTE_CARP_mobile_invert$YEAR == 2016,]
CARP_2008_data <- LTE_CARP_mobile_invert[LTE_CARP_mobile_invert$YEAR == 2008,]
CARP_2012_data <- LTE_CARP_mobile_invert[LTE_CARP_mobile_invert$YEAR == 2012,]
print(CARP_2012_data[1,1])


bc_func <- function(df_proxy)
{
  # #calc of bray-curtis
  # 
  sum_of_min <- sum(df_proxy$MIN_SPECIES_BIOMASS)
  sum_of_biomasses <- sum(df_proxy$CTRL_AFDM)+sum(df_proxy$ANNUAL_AFDM)
  bc_result <- 1-((2*sum_of_min)/sum_of_biomasses)
  return(bc_result)
}

proxy_matrix <- matrix(NA, nrow = dim(LTE_CARP_mobile_invert)[1],ncol = 6) #proxy matrix to fill with nas
output_df <- data.frame(proxy_matrix)
colnames(output_df) <- c("SITE", "YEAR", "MONTH", "DISM", "YEARS_SINCE_EST", "COARSE_GROUPING")
for(yr in years_vector)
{
  # print(yr)
  yr_same <- CARP_2012_data[CARP_2012_data$YEAR == yr,]
  for(m in months_vector)
  {
    site <- CARP_2012_data[1,1]
    
    m_same <- yr_same[yr_same$MONTH == m,]
    # print(m_same[1,1])
    if(nrow(m_same) == 0)
    {
      next
    }
    ctrl_df <- m_same[m_same[,5] == "CONTROL",]
    ctrl_df <- ctrl_df[,c("SCIENTIFIC_NAME","AFDM")]
    names(ctrl_df)[names(ctrl_df)== "AFDM"] <- "CTRL_AFDM" #rename
    # print(ctrl_df)
    
    annual_df <- m_same[m_same[,5]=="ANNUAL",] #same with anual
    annual_df <- annual_df[,c("SCIENTIFIC_NAME","AFDM")]
    names(annual_df)[names(annual_df)== "AFDM"] <- "ANNUAL_AFDM"
    # print(annual_df)
    
    dis_proxy <- merge(ctrl_df,annual_df, by = "SCIENTIFIC_NAME",all = TRUE) #make into one proxy matrix
    
    dis_proxy[,'MIN_SPECIES_BIOMASS'] = NA #add column and fill with nas
      for(j in 1:dim(dis_proxy)[1]){ #go row by row in the control and annual afdm
        for(k in c(2,3))
        {
          if(is.na(dis_proxy[j,k]) == TRUE) #if there isn't a species list it as 0
          {
            dis_proxy[j,k] <- 0
          } else {
            next
          }
        }
      }
    for(animal in 1:length(dis_proxy$MIN_SPECIES_BIOMASS)) #for all rows in the min species biomass column
        {
          dis_proxy$MIN_SPECIES_BIOMASS[animal] <- pmin(dis_proxy$CTRL_AFDM[animal],dis_proxy$ANNUAL_AFDM[animal]) #get the min amount from the control and annual columns
        }
    print(dis_proxy)
    bc <- bc_func(dis_proxy)
    
    row_counter = 1
    output_df$SITE[row_counter] <-CARP_2012_data$SITE[m]
    output_df$YEAR[row_counter] <- CARP_2012_data$YEAR[m]
    output_df$MONTH[row_counter] <- CARP_2012_data$MONTH[m]
    output_df$DISM[row_counter] <- bc
    output_df$YEARS_SINCE_EST[row_counter] <- CARP_2012_data$YEARS_SINCE_EST[m]
    output_df$COARSE_GROUPING[row_counter] <- CARP_2012_data$COARSE_GROUPING[m]
    
    row_counter = row_counter +1 
    print(output_df)
    # new_line <- data.frame(CARP_2008_data$SITE[i], CARP_2008_data$YEAR[i], CARP_2008_data$MONTH[i], bc, CARP_2008_data$YEARS_SINCE_EST[i], CARPd$COARSE_GROUPING[i])
    # names(new_line) <- c("SITE", "YEAR", "MONTH", "DISM", "YEARS_SINCE_EST", "COARSE_GROUPING")
# 
#     print(output_df)
  }
  
  # print(yr_same)
}
 




LTE_function <- function(df_used) #enter subset
{
  proxy_matrix <- matrix(NA, nrow = dim(LTE_CARP_mobile_invert)[1],ncol = 6) #proxy matrix to fill with nas
  output_df <- data.frame(proxy_matrix)
  colnames(output_df) <- c("SITE", "YEAR", "MONTH", "DISM", "YEARS_SINCE_EST", "COARSE_GROUPING") #name columns of dataframe

  library(dplyr)
  # months_seq <- seq(from = 7, to = 10, by = 1)
  # year_seq <- seq(from = 2008, to = 2023, by = 1)
  # 
  # print(dim(df_used)[1])
  for(yr in years_vector) #basically trying to isolate every month year pair
  {
    yr_same <- df_used[df_used$YEAR == yr,]
    for(m in months_vector)
    {
      site <- 
      # print(m)
      m_same <- yr_same[yr_same$MONTH == m,]
      print(m_same)
      ctrl_vector <- m_same[m_same$TREATMENT == "CONTROL",] #sort into control
      # print(ctrl_vector)
      # ctrl_vector <- ctrl_vector[,c("SCIENTIFIC_NAME","AFDM")] #only get these two columns
      # names(ctrl_vector)[names(ctrl_vector)== "AFDM"] <- "CTRL_AFDM" #rename
      # annual_vector <- yr_same[yr_same[i,5]=="ANNUAL",] #same with anual
      # annual_vector <- annual_vector[,c("SCIENTIFIC_NAME","AFDM")]
      # names(annual_vector)[names(annual_vector)== "AFDM"] <- "ANNUAL_AFDM"
    }
  }
  return(m_same)
}
  #   dis_proxy <- merge(ctrl_vector,annual_vector, all = TRUE) #make into one proxy matrix
  #   dis_proxy[,'MIN_SPECIES_BIOMASS'] = NA #add column and fill with nas
  #   # print("loop entered")
  #   for(j in 1:dim(dis_proxy)[1]){ #go row by row in the control and annual afdm
  #     for(k in c(2,3))
  #     {
  #       if(is.na(dis_proxy[j,k]) == TRUE) #if there isn't a species list it as 0
  #       {
  #         dis_proxy[j,k] <- 0
  #       } else {
  #         next
  #       }
  #     }
  #   }
  # }
  # 
  # 
  #   # print("loop exited")
  # 
  #   #WRITE IN HERE HOW TO SUBSTITUTE 0 FOR SPECIES BIOMASS NOT IN ONE BUT IN THE OTHER
  #   for(animal in 1:length(dis_proxy$MIN_SPECIES_BIOMASS)) #for all rows in the min species biomass column
  #   {
  #     dis_proxy$MIN_SPECIES_BIOMASS[animal] <- pmin(dis_proxy$CTRL_AFDM[animal],dis_proxy$ANNUAL_AFDM[animal]) #get the min amount from the control and annual columns
  #   }
  # #calc of bray-curtis
  # 
  #   sum_of_min <- sum(dis_proxy$MIN_SPECIES_BIOMASS)
  #   sum_of_biomasses <- sum(dis_proxy$CTRL_AFDM)+sum(dis_proxy$ANNUAL_AFDM)
  #   bray_curtis <- 1-((2*sum_of_min)/sum_of_biomasses)
  # 
  #   output_df$SITE[i] <-df_used$SITE[i]
  #   output_df$YEAR[i] <- df_used$YEAR[i]
  #   output_df$MONTH[i] <- df_used$MONTH[i]
  #   output_df$DISM[i] <- bray_curtis
  #   output_df$YEARS_SINCE_EST[i] <- df_used$YEARS_SINCE_EST[i]
  #   output_df$COARSE_GROUPING[i] <- df_used$COARSE_GROUPING[i]
    # new_line <- data.frame(df_used$SITE[i], df_used$YEAR[i], df_used$MONTH[i], bray_curtis, df_used$YEARS_SINCE_EST[i], df_used$COARSE_GROUPING[i])
    # names(new_line) <- c("SITE", "YEAR", "MONTH", "DISM", "YEARS_SINCE_EST", "COARSE_GROUPING")
    #
#   }
# 
# 
#   return(output_df)
# }

LTE_function(CARP_2016_data)
CARP_mobinv_dism <- output_df

