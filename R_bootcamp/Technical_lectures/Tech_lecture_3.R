rm(list = ls())

#random numbers = rnorm dis
rnorm(3,mean=0,sd=2) #number of random numbers, mean, sd
#histogram
hist(rnorm(10000,mean=0,sd=1),50) #data, number of bins
#runif = uniformly dist, rbinom = binomially dist, rpois = poisson dist
x <-rnorm(1000,0,1)
hist(x,50)
x<-rexp(1000,rate=1) #rate = controls shape of dist
hist(x,50)

x <- runif(1000,min=0,max=1)
hist(x,50)

#sample function = takes random sample/subset of a larger dataset
#with replacement = 'pull back before resampling' without replacement = 'pulls out' before resampling and keeps it
#replace = false means no repeats
#sample(vector,size of sample, replace (repeats or no repeats))


#random numbers are pseudo random
set.seed(1234567)
rnorm(10)


#Subsetting vectors
vect <- c('a','b','c','d','e','f','g','h','i')
vect[c(1,3)]
vect[1:3]
vect[seq(1,10,2)]
vect[c(1,1,2,2,3,3)]
#assigning vectors to other subsets of vectors
V1<-c(1,3,5)
V2<-c(101,102,103,104,105)
V2[c(1,2,4)]<-V1 #take the 01,02, and 04 elements of V2 and replace them with those values from V1
V1<- c(1,2)
V2<-c(101,102,103,104,105)
V2[c(1,2,4,5)]<-V1 #four elements of V2 > two elements of V1, so V1 elements are reused
V1<- c(1,2,3)
V2<-c(101,102,103,104,105)
V2[c(1,2,4,5)]<-V1 #will recycle back to the beginning, will not give warning unless it's not a multiple


#Matrices and arrays
x<- matrix(c(1,2,3,4),nrow=2,ncol=2) #data wanted, number of rows, number of columns
#arrays = 3d/many dimensions
x<- array(1:8,dim=c(2,2,2)) #can only display each splice, front face, back face, side
#column major order = down columns first then across rows, default but can tell r to do the reverse, by row = FALSE by default (true if you want columns)
#c (column major) and f (by row) in python
x<-matrix(c(1,2,3,4),nrow=2,ncol=2) 
#using recycling for having same columns repeat over and over again
x<- matrix(c(1,2),2,2)
#same thing if rows and columns is shorter than the length of the formers
x<- matrix(c(1,2),3,3)
#can pull out of any single element like x[i,j], same with dataframes
#can subset aka [i,] which is the ith row, x[,i] is the ith column, but both are converted to vectors
x<- matrix(c(1,2,3,4),2,2)
x
x[1,]
#can tell it not to by writing drop = FALSE
x[1,,drop=FALSE]

#naming columns and rows of matrix
x<- matrix(c(1,2,3,4),2,2)
colnames(x) <- c('nm1','nm2')
rownames(x) <- c('rnm1','rnm2')
x
#indexing by names harder to do in r, need to do it by boolean
x[rownames(x)=="rnm1"]
#matrix stored in linear format with dim showing what the dimensions are
#resizing
x<- matrix(1:6,2,3)
x
dim(x)
dim(x) <- c(3,2) #resizes
x
#column major order always applies, feeds same numbers in same way in column major order

#can use pdf() or jpeg() functions where all plot commands go there
#dev.off --> turns off default plotting device 
#save.image() saves all of variables
#getwd() finds what directory you're in
#load() loads workspace into r from save.image
#save single variable = save()
#saveRDS saves 1 variable
#source () runs file name and runs it
#$ --> lists, like a vectors but all heterogeneous and not the same type