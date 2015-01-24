## functions that cache the inverse of a matrix


##make a matrix object theat can be cache its inverse
makeCacheMatrix <- function(x = matrix()) {

  ##inverse property
  i<-NULL
  
  #set the matrix
  set<-function(matrix){
    m<<-matrix
    i<<-NULL
  }
  
  ##get the matrix
  get<-function(){
    #Return matrix
    m
  }

#set the inverse of a matrix
setInverse<-function(inverse){
  i<<-inverse  
}

##get inverse of matrix
getInverse<-function(){
  #Return inverse property
  i
}

##Return a list of the methods
list(set=set,get=get,
     setInverse=setInverse,
     getInverse=getInverse)
}


##Inverse of the special matrix returned by "makeCacheMatrix"
##above. If inverse has already being calculated (an matrix hasn't
##changed), then "cachesolve"should retrive the inverse from the cache
cacheSolve<-function(x,...){
  
  ##matrix that is inverse of x
  m<-x$getInverse()
  
  ##return the inverse if already set
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  ##get matrix from object
  data<-x$get()
  
  ##calculate inverse using matrix multiplication
  m<-solve(data)%%data
  
  ##set inverse of object
  x$setInverse(m)
  
  ##return matrix
  m
}
