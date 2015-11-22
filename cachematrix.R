## Put comments here that give an overall description of what your
## functions do:
##
## set value of the matrix
## get value of the matrix
## set the inverse of the value of the matrix
## get the inverse of the value of the matrix
##
## Write a short comment describing this function
## Follows example of makeVector.R substituting inverse for mean
## and matrix for vector

makeCacheMatrix <- function(x = matrix()) {
  inverseValue <- NULL
  set <- function(y) {
    x <<- y
    inverseValue <<- NULL
  }  
  
  get <- function() x
  setinverse <- function(inverse) inverseValue <<- inverse
  getinverse <- function() inverseValue
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
##
## Following the example of cachemean.R, substituting inverse for
## mean and using the solve function to compute the inverse of the
## matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inverseValue <- x$getinverse()
  if(!is.null(inverseValue)) {
    message("getting cached data")
    return(inverseValue)
  }
  data <- x$get()
  inverseValue <- solve(data, ...)
  x$setinverse(inverseValue)
  inverseValue
}