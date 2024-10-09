rm(list = ls())

#lecture 1, exercise 2, calculate tree height

distance_m <- c(40,20,30)
angle_deg <- c(37,15,50)
angle_rad <- angle_deg*pi/(180) #angle to radians
angle_rad

height_m <- distance_m*tan(angle_rad)
height_m

res <- mean(height_m)
res