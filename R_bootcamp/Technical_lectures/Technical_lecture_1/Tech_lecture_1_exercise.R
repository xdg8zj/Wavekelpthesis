rm(list = ls())
f <- read.csv('/Users/aleynaloughran-pierce/Desktop/CHMBE_Summer_2024/R_bootcamp/DataEtc/NagyEtAl1999.csv')
plot(log10(f$M.g), log10(f$FMR.kJ.day.1),type="n", main = "Practice Tech Lecture 1 chart", xlab = "Mass", ylab = "Metabolic rate (kJ/day)")
bird <- subset(f,Class == 'Aves')
mammal <- subset(f, Class == 'Mammalia')
reptile <- subset(f,Class == 'Reptilia' )
bird
points(log10(bird$M.g),log10(bird$FMR.kJ.day.1), col = "blue")
points(log10(mammal$M.g), log10(mammal$FMR.kJ.day.1), col = "red")
points(log10(reptile$M.g), log10(reptile$FMR.kJ.day.1), col ="green")

mean_bird_mass <- mean(bird$M.g)
min_bird_mass <- min(bird$M.g)
max_bird_mass <- max(bird$M.g)

mean_mammal_mass <- mean(mammal$M.g)
min_mammal_mass <- min(mammal$M.g)
max_mammal_mass <- max(mammal$M.g)


