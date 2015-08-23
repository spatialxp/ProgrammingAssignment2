## Contains a pair of functions that can be used to calculate the inverse of 
## a matrix. If the inverse has already been calculated it is returned from 
## the cached value, otherwise the inverse is calculated and cached.


makeCacheMatrix <- function(x = matrix()) {
  # Creates a special matrix object that can cache its inverse.
  #
  # Args:
  #   x: matrix
  #
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  # Returns the inverse of a matrix. If the inverse has already been solved it
  # is returned from a cached value, otherwise the inverse is calculated.
  #
  # Args:
  #   x: special matrix object created using the makeCacheMatrix function
  #
  
  inv <- x$getinverse()
  if(is.null(inv)) {
    message("calculating inverse of matrix")
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
  }
  inv
}
