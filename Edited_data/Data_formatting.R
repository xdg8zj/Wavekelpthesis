rm(list = ls())

LTER_data <- read.csv("/Users/aleynaloughran-pierce/Desktop/CHMBE_Summer_2024/Raw Files/Annual_All_Species_Biomass_at_transect_20240501.csv")

LTER_mobile_inverts <- subset(LTER_data, COARSE_GROUPING == "MOBILE INVERT")
LTER_sessile_inverts <- subset(LTER_data, COARSE_GROUPING == "SESSILE INVERT")
LTER_understory_algae <- subset(LTER_data, COARSE_GROUPING == "UNDERSTORY ALGAE")
LTER_canopy_algae <- subset(LTER_data, COARSE_GROUPING == "GIANT KELP")

CDIP_wave_data <- read.csv("/Users/aleynaloughran-pierce/Desktop/CHMBE_Summer_2024/Edited data/CDIP_Waveperiods.csv")

LTE_data_all <- read.csv("/Users/aleynaloughran-pierce/Desktop/CHMBE_Summer_2024/Raw Files/LTE_All_Species_Biomass_at_transect_20240501.csv")
LTE_mobile_inverts <- subset(LTE_data_all, GROUP == "INVERT" & MOBILITY == "MOBILE")
LTE_sessile_inverts <- subset(LTE_data_all, GROUP == "INVERT" & MOBILITY == "SESSILE")
LTE_understory_algae <- subset(LTE_data_all, GROUP == "ALGAE" & SP_CODE != "MAPY")
LTE_canopy_algae <-subset(LTE_data_all, GROUP == "ALGAE" & SP_CODE == "MAPY")

ctrl_mobile_inverts <- subset(LTE_mobile_inverts, TREATMENT =="CONTROL")
ctrl_sessile_inverts <- subset(LTE_sessile_inverts, TREATMENT == "CONTROL")
ctrl_understory_algae <- subset(LTE_understory_algae, TREATMENT == "CONTROL")
ctrl_canopy_algae <- subset(LTE_canopy_algae, TREATMENT == "CONTROL")

removal_mobile_inverts <- subset(LTE_mobile_inverts, TREATMENT == "ANNUAL" )
removal_sessile_inverts <- subset(LTE_sessile_inverts, TREATMENT == "ANNUAL")
removal_understory_algae <- subset(LTE_understory_algae, TREATMENT == "ANNUAL")
removal_canopy_algae <- subset(LTE_canopy_algae, TREATMENT == "ANNUAL")







