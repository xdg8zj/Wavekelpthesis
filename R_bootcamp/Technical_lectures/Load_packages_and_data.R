rm(list = ls())

load("/Users/aleynaloughran-pierce/Desktop/CHMBE_Summer_2024/R_bootcamp/DataEtc/GPDDFiltered.RData")
head(gpdd)
tail(gpdd)

require(maps) #loads package 
map()
points(gpdd$long,gpdd$lat,type='p',col='orange1')
