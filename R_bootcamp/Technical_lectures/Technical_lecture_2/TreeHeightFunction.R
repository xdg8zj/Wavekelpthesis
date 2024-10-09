rm(list = ls())

#Example function: Calculates height on tree based on angle elevation and distance
# degrees = numeric vector of elevation measurements in degrees
#distance = numeric vector of distance measurements in any unit should typically be a vector of the same length as degrees
#Output = height of tree in the same units as distance
TreeHeight <- function(degrees, distance) #makes a function, uses degrees and distance as its two inputs
{
  radians <- degrees * pi /180 #assigns what radians are from conversion to degrees
  height <- distance * tan(radians) #calculates height from distance inputs
  return (height)
}

TreeHeight(37,40) #what we're putting in
TreeHeight(c(37,25,31),40) #can have vectors of more than one value

#loops
for(counter in 1:10) #need vector
{
  print("Hello world!")
}

for(year in 2009:2001)
{
  print(paste('The year is', year))
}

#ex: for(species in c(species 1, species 2, species 3...etc))

#writing a loop to find out what !10 is
ten_factorial <- 1 #you need to initialize it
factorial_numbers <- 1:10

for(i in factorial_numbers)
{
  ten_factorial <- ten_factorial*i
  print(ten_factorial)
}
  
#loop exercise
reverse_letter_names <- LETTERS[26:1]
res <- ""
for(i in reverse_letter_names)
{
  res <- paste(res,i,sep="")
}
print(res)

#fix later
X <- rep(0,10) #go back and check this
X[1] <- 10
for(num in X)
{
  new_number <- n
}

#fix later
year_date <- 1980:1990
month_date <- 1:7

for(years in year_date)
{
    for(month_dates in month_date)
    {
      months <- paste("Month",month_date, sep = " ")
  }
  year_and_months <- c(paste("Year", years, months, sep = " "))
  print(year_and_months)
  
}


#--Ecological examples
Exponential <- function(N0 =1, r = 1, generations = 10)
{
  #runs a simulation of exponential growth
  #returns a vector of length generations
  
  N<- rep(NA, generations) #creates a vector of NA
  
  N[1] <- N0 #rep
  for(t in 2:generations)
  {
    N[t] <- N[t-1]*exp(r)
  }
  return(N)
  
}
plot(Exponential(), type="l", main = "Exponential growth")
Exponential(N0 = 2, generations = 12) #will use user input for these ones but not for 


