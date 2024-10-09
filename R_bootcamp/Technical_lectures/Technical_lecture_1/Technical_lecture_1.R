rm(list = ls())
x <- 2
length(x)
wing.length.cm <- c(4.7,5.2,4.8)
length(wing.length.cm)

wing.length.cm
wing.length.cm[3]
wing.length.cm[3] <- 3.3
wing.length.cm


wing.width.cm <- c(5,10.3,15,20.7)
wing.width.mm <- wing.width.cm*10
wing.width.cm
wing.width.mm

#EXERCISE 0:
mass.kg <- c(10,1.2,2.12,1001) #assigns numbers to vectors
mass.kg #prints vector
mass.kg[3] #prints number in vector
mass.kg[3] <- 2.13 #reassigns 3rd element in mass.kg
mass.kg <- c(mass.kg, 101.2, 233.4) #adds numbers to end of vector
mass.kg
#rm = remove variable
#ls = list
#rm(list = ls()) = clears work space, can put it on top of script

mean(wing.length.cm)
min(wing.length.cm)
max(wing.length.cm)
sum(wing.length.cm)

#sequences
years <- 1990:2009
years
length(years)
years <- 2009:1990
years
#seq() = (# start, # end, increment), can also write seq() = (from = start, to = end, by = increment) OR seq() = (from = start, by = increment, to = end)
seq(from =1, to = 5, by = 0.1)

#class() tells you what class it is
x <- "test"
class(x)
species_name <- "Quercus robur"
species_name
species_name <- 'Fraxinus excelsior'
species_name
species_name*10
answer <- 42
paste('The answer is, ', answer) #single string
paste('The year is', 1990:2000)
x <- c("this", "is", "a", "test") #vector of strings
#for paste, sep = will determine what the separation is, aka sep = " " is separated by a space
length(x)


#plot graphs
plot(x = 1:25, y = (1:25)^2, main="A simple graph", xlab = "x label", ylab = "y label", pch =1:25, col = "violetred4")
plot(x = 1:25, y = (1:25)^2, main="A simple graph", xlab = "x label", ylab = "y label", type="o", col = c("turquoise1","thistle2", "snow2"))
points(5,50,col = "wheat3")
lines(2:5, (5:2)^2, col = "green")
points(2:26, (26:2)^1/2, col= "purple")
#first argument = x values, second argument = y values, main = title

d <- read.csv('/Users/aleynaloughran-pierce/Desktop/CHMBE_Summer_2024/R_bootcamp/DataEtc/NagyEtAl1999.csv') #reads in file from directory
#'..' = go up one in your directory
#look into using relative addressing
class(d)
head(d,10)
tail(d,10)
ncol(d) #number of columns
nrow(d) #number of rows
d$M.g #gives vector of all the masses in grams
d$Species
d["M.g"] #one column data frame
d[["M.g"]] #turns into vector
d[,5] 
plot(d$M.g, d$FMR.kJ.day.1)
plot(log10(d$M.g), log10(d$FMR.kJ.day.1), type = 'p') #log by itself is just natural log

#subset function = subset(f, things you want to choose == column name)
reptiles <- subset(d, Class == "Reptilia")
d$Class == "Reptilia"
dim(reptiles)
head(reptiles)
reptiles <- d[d$Class == "Reptilia",]
d$Class == "Reptillia"

small.reptiles <- subset(d,'Reptilia' == Class & d$M.g<100) #use & instead of AND
partial.small.reptiles <_ d[(d$Class == "Reptilia")&(d$M.g<10), c("Species", "M.g")]



