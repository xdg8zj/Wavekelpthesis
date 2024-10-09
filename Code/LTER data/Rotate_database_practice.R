rm(list = ls())

df <- structure(list(Var=c("A","A","A","A","A"),Date = c("1-1","1-2","1-3","1-4","1-5"),Score = c(5L,6L,2L,9L,4L)),class="data.frame",row.names=(c(NA,-5L)))

library(dplyr)
library(tidyr)
df %>% separate(Date,c('V1','V2')) %>%
  mutate(V2=paste0('t',V2)) %>%
  pivot_wider(names_from = V2,values_from=Score) %>% select(-V1)
