rm(list = ls())

#Ricker model of pop growth
#Nt = variable (input)
# r = intrinsic rate of increase -- variable (input)
# K = carrying capacity -- variable (input)
#e = euler's number -- variable (constant)
#t = changing/set of values -- vector (input), years
#Nt+1 = new population (output) -- likely vector

Ricker_model <- function(N0 = 2, r= 1.2, K= 50, yrs= 10)
{
  N <- rep(NA,yrs) #creates empty vector of NAs and length of years (like making blank array)
  N[1] <- N0 #sets first ting to N0
  for(t in 1:(yrs-1)) #for thing in list of 1 to t
  {
    N[t+1] <- N[t]*(exp(1)^(r*(1-(N[t]/K)))) #next pop equation for each t
#
  }
  
  return(N)
}