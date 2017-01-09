####################################
##Slawa Rokicki
##June 8, 2014
##Harvard University
##Writing and debugging functions
####################################

#basic syntax
square.it<-function(x){
  square<-x*x
  return(square)
}

#square a number
square.it(5)
#square a vector
square.it(c(1,4,2))
#square a character (not going to happen)
square.it("hi")

#save a matrix, then square it
matrix1<-cbind(c(3,10),c(4,5))
square.it(matrix1)

#local vs global environment

fun1<-function(x){
  3*x-1
}
fun1(5)
fun2<-function(x){
  y <- 3*x-1
}
fun2(5)

#more complex

my.fun <- function(X.matrix, y.vec, z.scalar){
  
  #use my previous function square.it() to square the scalar and save result
  sq.scalar<-square.it(z.scalar)
  
  #multiply the matrix by the vector using %*% operator
  mult<-X.matrix%*%y.vec
  
  #multiply the two resulting objects together to get a final object
  final<-mult*sq.scalar
  
  #return the result
  return(final)
}

#save a matrix and a vector object
my.mat<-cbind(c(1,3,4),c(5,4,3))
my.vec<-c(4,3)

#pass my.mat and my.vec into the my.fun function 
my.fun(X.matrix=my.mat, y.vec=my.vec, z.scalar=9)
#this is the same as
my.fun(my.mat, my.vec, 9)

#return a list
another.fun<-function(sq.matrix, vector){
  
  #transpose matrix and square the vector
  step1<-t(sq.matrix)
  step2<-vector*vector
  
  #save both results in a list and return
  final<-list(step1, step2)
  return(final)
}

#call the function and save result in object called outcome
outcome<-another.fun(sq.matrix=cbind(c(1,2),c(3,4)), vector=c(2,3))

#print the outcome list
print(outcome)

###extract first in list
outcome[[1]]

##extract second in list
outcome[[2]]

#Debugging

#error when you do this
my.fun(X.matrix=my.mat, y.vec=c(2,3,6,4,1), z.scalar=9)

#use debug to find error
debug(my.fun)
my.fun(X.matrix=my.mat, y.vec=c(2,3,6,4,1), z.scalar=9)

#print out what is happening (sanity checks)
my.fun <- function(X.matrix, y.vec, z.scalar){
  print("xmatrix")
  print(X.matrix)
  
  print("yvec")
  print(y.vec)
  
  print("Dimensions")
  print(dim(X.matrix))
  print(length(y.vec))
  
  #use my previous function square.it() to square the scalar and save result
  sq.scalar<-square.it(z.scalar)
  print(paste("sq.scalar=", sq.scalar))
  
  #multiply the matrix by the vector using %*% operator
  mult<-X.matrix%*%y.vec
  
  #multiply the two resulting objects together to get a final object
  final<-mult*sq.scalar
  
  #return the result
  return(final)
}

my.fun(X.matrix=my.mat, y.vec=c(2,3,6,4,1), z.scalar=9)

#use stop function
my.second.fun<-function(matrix, vector){
  
  if(dim(matrix)[2]!=length(vector)){
    stop("Can't multiply matrix%*%vector because the dimensions are wrong")
  }
  
  product<-matrix%*%vector
  
  return(product)
  
}

#function works when dimensions are right
my.second.fun(my.mat, c(6,5))
#function call triggered error
my.second.fun(my.mat, c(6,5,7))



