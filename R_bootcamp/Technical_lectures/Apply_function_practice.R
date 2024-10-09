rm(list = ls())

m<-matrix(1:12,3,4)
apply(FUN=sum,X=m,MARGIN = 2) #1 = rows, 2 = columns, fun = function applied, X = what it's being applied to
