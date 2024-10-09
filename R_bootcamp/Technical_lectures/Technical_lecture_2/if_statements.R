rm(list = ls())

#if statements

x <-2
if(x==3)
{
  print("If triggered")
}

even_or_odd <- function(integer)
{
  if (integer%%2 == 0)
  {
    print("even")
  }
  else
  {
    print("odd")
  }
}

even_or_odd(5)
even_or_odd(40)


power_func <-function(integer)
{
  if(log2(integer)%%1 == 0)
  {
    print("power")
  }
  else
  {
    print("not power")
  }
}
power_func(1)



prime_func <- function(integer=0)
{
  if(integer == 1)
  {
    return("unit")
  }
  if (integer == 0)
  {
    return("zero")
  }
  for(f in 2:sqrt(integer))
  {
    if (integer%%f == 0) 
    {
      return("composite")
    }
  }
  return("prime")
}