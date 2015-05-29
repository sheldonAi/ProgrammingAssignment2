#load MASS package
#to use ginv() computing the inverse matrix
library(MASS)

## makeCacheMatrix: This function creates a special
##"matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  im<-NULL
  set<-function(y=matrix()){
    x<<-y
    im<<-NULL
  }
  get<-function() x
  setInverse<-function(inverse) im<<-inverse
  getInverse<-function() im
  list(set=set,get=get,
       setInverse=setInverse,getInverse=getInverse)
}


##cacheSolve: This function computes the inverse of
##the special "matrix" returned by makeCacheMatrix 
##above. If the inverse has already been calculated 
##(and the matrix has not changed), then the 
##cachesolve should retrieve the inverse from the 
##cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  im <- x$getInverse()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  #im <- solve(data)
  #solve() can only be used to square matrix
  #use ginv() compute inverse matrix
  im<-ginv(data)
  x$setInverse(im)
  im
}
