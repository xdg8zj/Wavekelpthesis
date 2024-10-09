#mixed effects model (LTER)
#Load the LTER_data.R file first
library(lme4)

#max waves
mobile_invert_mem_max <- lmer(site_subset$`MOBILE INVERT`~site_subset$WINTER_MAX_HS_M + (1|site_subset$SITE)+(1|site_subset$YEAR), data = site_subset) #LEM with waves as fixed affect and site and year as random
summary(mobile_invert_mem_max)
mobile_max_1 <- lmer(site_subset$`MOBILE INVERT`~site_subset$WINTER_MAX_HS_M + (1|site_subset$SITE), data = site_subset) #set year dropped
anova(mobile_max_1, mobile_invert_mem_max) #does year matter?
mobile_max_2 <- lmer(site_subset$`MOBILE INVERT`~site_subset$WINTER_MAX_HS_M + (1|site_subset$YEAR), data = site_subset)
anova(mobile_max_2, mobile_invert_mem_max) #does site matter?
anova(mobile_max_1,mobile_max_2) #any difference between the two?
mobile_no_max <- lmer(site_subset$`MOBILE INVERT`~1 + (1|site_subset$SITE)+(1|site_subset$YEAR), data = site_subset)
anova(mobile_invert_mem_max,mobile_no_max)
coefficients(mobile_invert_mem_max)
#-2.149692

#mobile mean waves
mobile_invert_mem_mean <- lmer(site_subset$`MOBILE INVERT`~site_subset$WINTER_MEAN_HS_M + (1|site_subset$SITE)+(1|site_subset$YEAR), data = site_subset)
summary(mobile_invert_mem_mean)
mobile_mean_1 <- lmer(site_subset$`MOBILE INVERT`~site_subset$WINTER_MEAN_HS_M + (1|site_subset$SITE), data = site_subset)
anova(mobile_mean_1, mobile_invert_mem_mean)
mobile_mean_2 <- lmer(site_subset$`MOBILE INVERT`~site_subset$WINTER_MEAN_HS_M + (1|site_subset$YEAR), data = site_subset)
anova(mobile_mean_2, mobile_invert_mem_mean)
anova(mobile_mean_1,mobile_mean_2)
mobile_no_mean <- lmer(site_subset$`MOBILE INVERT`~1 + (1|site_subset$SITE)+(1|site_subset$YEAR), data = site_subset)
anova(mobile_invert_mem_mean,mobile_no_mean)
coefficients(mobile_invert_mem_mean)
#-7.491389

#epilithic
#max waves
epi_sessile_invert_mem_max <- lmer(site_subset$`EPILITHIC SESSILE INVERT`~site_subset$WINTER_MAX_HS_M + (1|site_subset$SITE)+(1|site_subset$YEAR), data = site_subset)
summary(epi_sessile_invert_mem_max)
epi_sessile_max_1 <- lmer(site_subset$`EPILITHIC SESSILE INVERT`~site_subset$WINTER_MAX_HS_M + (1|site_subset$SITE), data = site_subset)
anova(epi_sessile_max_1, epi_sessile_invert_mem_max)
epi_sessile_max_2 <- lmer(site_subset$`EPILITHIC SESSILE INVERT`~site_subset$WINTER_MAX_HS_M + (1|site_subset$YEAR), data = site_subset)
anova(epi_sessile_max_2, epi_sessile_invert_mem_max)
anova(epi_sessile_max_1,epi_sessile_max_2)
epi_sessile_no_max <- lmer(site_subset$`EPILITHIC SESSILE INVERT`~1 + (1|site_subset$SITE)+(1|site_subset$YEAR), data = site_subset)
anova(epi_sessile_invert_mem_max,epi_sessile_no_max)
coefficients(epi_sessile_invert_mem_max)
#-0.3329947

#epi mean waves
epi_sessile_invert_mem_mean <- lmer(site_subset$`EPILITHIC SESSILE INVERT`~site_subset$WINTER_MEAN_HS_M + (1|site_subset$SITE)+(1|site_subset$YEAR), data = site_subset)
summary(epi_sessile_invert_mem_mean)
epi_sessile_mean_1 <- lmer(site_subset$`EPILITHIC SESSILE INVERT`~site_subset$WINTER_MEAN_HS_M + (1|site_subset$SITE), data = site_subset)
anova(epi_sessile_mean_1, epi_sessile_invert_mem_mean)
epi_sessile_mean_2 <- lmer(site_subset$`EPILITHIC SESSILE INVERT`~site_subset$WINTER_MEAN_HS_M + (1|site_subset$YEAR), data = site_subset)
anova(epi_sessile_mean_2, epi_sessile_invert_mem_mean)
anova(epi_sessile_mean_1,epi_sessile_mean_2)
epi_sessile_no_mean <- lmer(site_subset$`EPILITHIC SESSILE INVERT`~1 + (1|site_subset$SITE)+(1|site_subset$YEAR), data = site_subset)
anova(epi_sessile_invert_mem_mean,epi_sessile_no_mean)
coefficients(epi_sessile_invert_mem_mean)
#-13.83985


#endolithic sessile inverts
#max waves
endo_sessile_invert_mem_max <- lmer(site_subset$`ENDOLITHIC SESSILE INVERT`~site_subset$WINTER_MAX_HS_M + (1|site_subset$SITE)+(1|site_subset$YEAR), data = site_subset)
summary(endo_sessile_invert_mem_max)
endo_sessile_max_1 <- lmer(site_subset$`ENDOLITHIC SESSILE INVERT`~site_subset$WINTER_MAX_HS_M + (1|site_subset$SITE), data = site_subset)
anova(endo_sessile_max_1, endo_sessile_invert_mem_max)
endo_sessile_max_2 <- lmer(site_subset$`ENDOLITHIC SESSILE INVERT`~site_subset$WINTER_MAX_HS_M + (1|site_subset$YEAR), data = site_subset)
anova(endo_sessile_max_2, endo_sessile_invert_mem_max)
anova(endo_sessile_max_1,endo_sessile_max_2)
endo_sessile_no_max <- lmer(site_subset$`ENDOLITHIC SESSILE INVERT`~1 + (1|site_subset$SITE)+(1|site_subset$YEAR), data = site_subset)
anova(endo_sessile_invert_mem_max,endo_sessile_no_max)
coefficients(endo_sessile_invert_mem_max)
#2.391642

endo_sessile_invert_mem_mean <- lmer(site_subset$`ENDOLITHIC SESSILE INVERT`~site_subset$WINTER_MEAN_HS_M + (1|site_subset$SITE)+(1|site_subset$YEAR), data = site_subset)
summary(endo_sessile_invert_mem_mean)
endo_sessile_mean_1 <- lmer(site_subset$`ENDOLITHIC SESSILE INVERT`~site_subset$WINTER_MEAN_HS_M + (1|site_subset$SITE), data = site_subset)
anova(endo_sessile_mean_1, endo_sessile_invert_mem_mean)
endo_sessile_mean_2 <- lmer(site_subset$`ENDOLITHIC SESSILE INVERT`~site_subset$WINTER_MEAN_HS_M + (1|site_subset$YEAR), data = site_subset)
anova(endo_sessile_mean_2, endo_sessile_invert_mem_mean)
anova(endo_sessile_mean_1,endo_sessile_mean_2)
endo_sessile_no_mean <- lmer(site_subset$`ENDOLITHIC SESSILE INVERT`~1 + (1|site_subset$SITE)+(1|site_subset$YEAR), data = site_subset)
anova(endo_sessile_invert_mem_mean,endo_sessile_no_mean)
coefficients(endo_sessile_invert_mem_mean)
# -49.61373

#understory algae
#max waves
understory_algae_mem_max <- lmer(site_subset$`UNDERSTORY ALGAE`~site_subset$WINTER_MAX_HS_M + (1|site_subset$SITE)+(1|site_subset$YEAR), data = site_subset)
summary(understory_algae_mem_max)
understory_algae_max_1 <- lmer(site_subset$`UNDERSTORY ALGAE`~site_subset$WINTER_MAX_HS_M + (1|site_subset$SITE), data = site_subset)
anova(understory_algae_max_1, understory_algae_mem_max)
understory_algae_max_2 <- lmer(site_subset$`UNDERSTORY ALGAE`~site_subset$WINTER_MAX_HS_M + (1|site_subset$YEAR), data = site_subset)
anova(understory_algae_max_2, understory_algae_mem_max)
anova(understory_algae_max_1,understory_algae_max_2)
understory_algae_no_max <- lmer(site_subset$`UNDERSTORY ALGAE`~1 + (1|site_subset$SITE)+(1|site_subset$YEAR), data = site_subset)
anova(understory_algae_mem_max,understory_algae_no_max)
coefficients(understory_algae_mem_max)
#7.345306
#mean waves
understory_algae_mem_mean <- lmer(site_subset$`UNDERSTORY ALGAE`~site_subset$WINTER_MEAN_HS_M + (1|site_subset$SITE)+(1|site_subset$YEAR), data = site_subset)
summary(understory_algae_mem_mean)
understory_algae_mean_1 <- lmer(site_subset$`UNDERSTORY ALGAE`~site_subset$WINTER_MEAN_HS_M + (1|site_subset$SITE), data = site_subset)
anova(understory_algae_mean_1, understory_algae_mem_mean)
understory_algae_mean_2 <- lmer(site_subset$`UNDERSTORY ALGAE`~site_subset$WINTER_MEAN_HS_M + (1|site_subset$YEAR), data = site_subset)
anova(understory_algae_mean_2, understory_algae_mem_mean)
anova(understory_algae_mean_1,understory_algae_mean_2)
understory_algae_no_mean <- lmer(site_subset$`UNDERSTORY ALGAE`~1 + (1|site_subset$SITE)+(1|site_subset$YEAR), data = site_subset)
anova(understory_algae_mem_mean,understory_algae_no_mean)
coefficients(understory_algae_mem_mean)
#37.05137


#GIANT KELP
#MAX
giant_kelp_mem_max <- lmer(site_subset$`GIANT KELP`~site_subset$WINTER_MAX_HS_M + (1|site_subset$SITE)+(1|site_subset$YEAR), data = site_subset)
summary(giant_kelp_mem_max)
giant_kelp_max_1 <- lmer(site_subset$`GIANT KELP`~site_subset$WINTER_MAX_HS_M + (1|site_subset$SITE), data = site_subset)
anova(giant_kelp_max_1, giant_kelp_mem_max)
giant_kelp_max_2 <- lmer(site_subset$`GIANT KELP`~site_subset$WINTER_MAX_HS_M + (1|site_subset$YEAR), data = site_subset)
anova(giant_kelp_max_2, giant_kelp_mem_max)
anova(giant_kelp_max_1,giant_kelp_max_2)
giant_kelp_no_max <- lmer(site_subset$`GIANT KELP`~1 + (1|site_subset$SITE)+(1|site_subset$YEAR), data = site_subset)
anova(giant_kelp_mem_max,giant_kelp_no_max)
coefficients(giant_kelp_mem_max)
#-6.056339

#MEAN WAVES
giant_kelp_mem_mean <- lmer(site_subset$`GIANT KELP`~site_subset$WINTER_MEAN_HS_M + (1|site_subset$SITE)+(1|site_subset$YEAR), data = site_subset)
summary(giant_kelp_mem_mean)
giant_kelp_mean_1 <- lmer(site_subset$`GIANT KELP`~site_subset$WINTER_MEAN_HS_M + (1|site_subset$SITE), data = site_subset)
anova(giant_kelp_mean_1, giant_kelp_mem_mean)
giant_kelp_mean_2 <- lmer(site_subset$`GIANT KELP`~site_subset$WINTER_MEAN_HS_M + (1|site_subset$YEAR), data = site_subset)
anova(giant_kelp_mean_2, giant_kelp_mem_mean)
anova(giant_kelp_mean_1,giant_kelp_mean_2)
giant_kelp_no_mean <- lmer(site_subset$`GIANT KELP`~1 + (1|site_subset$SITE)+(1|site_subset$YEAR), data = site_subset)
anova(giant_kelp_mem_mean,giant_kelp_no_mean)
coefficients(giant_kelp_mem_mean)
#-62.38117


#fish data
#max waves
fish_mem_max <- lmer(site_subset$`FISH`~site_subset$WINTER_MAX_HS_M + (1|site_subset$SITE)+(1|site_subset$YEAR), data = site_subset)
summary(fish_mem_max)
fish_max_1 <- lmer(site_subset$`FISH`~site_subset$WINTER_MAX_HS_M + (1|site_subset$SITE), data = site_subset)
anova(fish_max_1, fish_mem_max)
fish_max_2 <- lmer(site_subset$`FISH`~site_subset$WINTER_MAX_HS_M + (1|site_subset$YEAR), data = site_subset)
anova(fish_max_2, fish_mem_max)
anova(fish_max_1,fish_max_2)
fish_no_max <- lmer(site_subset$`FISH`~1 + (1|site_subset$SITE)+(1|site_subset$YEAR), data = site_subset)
anova(fish_mem_max,fish_no_max)
coefficients(fish_mem_max)
#-1.712508

#mean waves
fish_mem_mean <- lmer(site_subset$`FISH`~site_subset$WINTER_MEAN_HS_M + (1|site_subset$SITE)+(1|site_subset$YEAR), data = site_subset)
summary(fish_mem_mean)
fish_mean_1 <- lmer(site_subset$`FISH`~site_subset$WINTER_MEAN_HS_M + (1|site_subset$SITE), data = site_subset)
anova(fish_mean_1, fish_mem_mean)
fish_mean_2 <- lmer(site_subset$`FISH`~site_subset$WINTER_MEAN_HS_M + (1|site_subset$YEAR), data = site_subset)
anova(fish_mean_2, fish_mem_mean)
anova(fish_mean_1,fish_mean_2)
fish_no_mean <- lmer(site_subset$`FISH`~1 + (1|site_subset$SITE)+(1|site_subset$YEAR), data = site_subset)
anova(fish_mem_mean,fish_no_mean)
coefficients(fish_mem_mean)
#-15.39134
