# 1.
# calculate rock cover for each transect
# the remaining cover is sand, such that: 100 - rock = sand

rock_tran <- substrate %>% 
  group_by(SITE, DATE, TRANSECT) %>% 
  filter(SUBSTRATE_TYPE == 'B' | SUBSTRATE_TYPE == 'BL' | SUBSTRATE_TYPE == 'BM' | SUBSTRATE_TYPE == 'BS' | SUBSTRATE_TYPE == 'C'| SUBSTRATE_TYPE == 'SS' | SUBSTRATE_TYPE == 'SH') %>% 
  summarize(rock = sum(PERCENT_COVER)/4) %>% 
  mutate(YEAR = year(DATE)) %>%
  mutate(y = as.character(YEAR), s = as.character(SITE), t = as.character(TRANSECT)) %>% 
  unite('SITE_YEAR', s:y)%>% 
  unite('SITE_YEAR_TRAN', SITE_YEAR:t)


# 2.
# calculate bray curtis dissimilarity using a Hellinger transformation

# wrangle for variables of interest
# remove sites that start later in the dataset
# start at 2002 when most transects were established
com_spp <- com %>% 
  filter(YEAR > 2001, COARSE_GROUPING != "FISH", SITE != "SCDI", SITE != "SCTW") %>%
  filter(SITE_TRAN != 'IVEE_3', SITE_TRAN != 'IVEE_5', SITE_TRAN != 'IVEE_6', SITE_TRAN != 'IVEE_7', SITE_TRAN != 'IVEE_8') %>%
  select(SITE, TRANSECT, YEAR, SITE_YEAR_TRAN, SP_CODE, rock,  DM_GM2) %>% 
  pivot_wider(names_from = 'SP_CODE', values_from = 'DM_GM2') %>% 
  arrange(SITE_YEAR_TRAN)

# dt of transects and rock for that year
com_sites <- com_spp %>% 
  select(SITE, TRANSECT, YEAR, SITE_YEAR_TRAN, rock)

# find relative abundance of each species
# hellinger trans takes sq root of relative abundance to downweight dominant spp.
com_spp_rel <- decostand(com_spp[,6:ncol(com_spp)], method = "hellinger", na.rm = TRUE)

row.names(com_spp_rel) <- com_sites$SITE_YEAR_TRAN

# calculate distance matrix
com_distmat <- vegdist(com_spp_rel, method = "bray", na.rm = TRUE) %>% 
  as.matrix()

# convert to a 3 column data frame
rowCol <- expand.grid(rownames(com_distmat), colnames(com_distmat))

labs <- rowCol[as.vector(upper.tri(com_distmat,diag=F)),]
dist_df <- cbind(labs, com_distmat[upper.tri(com_distmat,diag=F)])
colnames(dist_df) <- c("Row","Col","bc")

# format and arrange df
com_dist_df <- dist_df %>% 
  separate(col = Row, sep = "_", into = c("SITE_1", "YEAR_1", "TRANSECT_1")) %>% 
  separate(col = Col, sep = "_", into = c("SITE_2", "YEAR_2", "TRANSECT_2")) %>% 
  arrange(SITE_1, SITE_2, TRANSECT_1, TRANSECT_2, YEAR_1, YEAR_2) %>% 
  mutate(YEAR_1 = as.numeric(YEAR_1), 
         YEAR_2 = as.numeric(YEAR_2),
         TRANSECT_1 = as.double(TRANSECT_1),
         TRANSECT_2 = as.double(TRANSECT_2)) %>% 
  select(SITE_1, SITE_2, TRANSECT_1, TRANSECT_2, YEAR_1, YEAR_2, bc) 
