rm(list = ls())

matrix_practce <- function(m) {
  #matrix_sub <- (NA, length(col(m)), length(row(m)))
  m2 <- matrix(NA,dim(m)[1], dim(m)[2]) 
  m2 <- m[nrow(m):1,]
  return(m2)
  }
x <- matrix(1:5,5,2)
matrix_practce(x)
x[1,]
x[c(1,2),]
x[c(2,1),]
#matrix practice
#matflip <- function(m) {
#for (mrow in 1:nrow(m)){
#     m2[nrow(m2)-mrowind+1,] <- m[mrowind,]
#     }
# return(m2)
#}

matflip3 <- function(m){
  return(m[rev(1:nrow(m)),])
}
matflip3(x)

