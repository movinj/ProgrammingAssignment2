## Put comments here that give an overall description of what your
## functions do

## Given a matrix, returns an object that contains functions that can be used to set the matrix, ...
## get its value, set its inverse, or get the cached value of its inverse

makeCacheMatrix <- function(x = matrix()) {
  ##initialize the 'inverse' variable with NULL
  inverse <- NULL
  
  ##function to set the value of the matrix x
  set <- function(y) {
    x <<- y
    
    ##need to empty the cache since the matrix itself has been updated
    inverse <<- NULL
  }
  
  ##function to get the value of the matrix
  get <- function() x
  
  ##function to set the variable 'inverse'
  setinverse <- function(inv) inverse <<- inv
  
  ##function to get the value of the inverse of x
  getinverse <<- function() inverse
  
  ##return a named list containing all the four functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Given an object (as created by the function above), checks if the inverse exists in the cache. If it does,
## returns the inverse. If not, calculates the inverse afresh, stores it in the cache, and returns it

cacheSolve <- function(x, ...) {
  ##Obtain the existing value of the cache
  inverse <- x$getinverse()
  
  ##If the existing cached value is not null, just return the value after printing a message
  if(!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  
  ##Otherwise, calculate the inverse afresh using 'solve' function
  data <- x$get()
  inverse <- solve(data, ...)
  
  ##set the cached value to the inverse that was just calculated
  x$setinverse(inverse)
  
  ##return the inverse
  inverse
}
