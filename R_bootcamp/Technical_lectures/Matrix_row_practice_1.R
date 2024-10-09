rm(list = ls())


matrix_practice <- matrix(c(1:9),3,3)
matrix_practice
matrix_practice <- matrix(c(1:9),3,3, byrow = TRUE)

row1 <- 1:4
row2 <- seq(from = 0.5, to = 0.2, by = -0.1)
row3 = c(2,3,5,7)

#matrixpractice2 <- matrix(NA,3,4)
#matrixparactice2[1,] <- 1:4
#matrixpractice2[2,] <-seq(...)
#matrixpractice2[3,] <- prime numbers

matrix_practice_2 <- matrix(c(row1,row2,row3),3,4, byrow = TRUE)
matrix_practice_2[1,]
