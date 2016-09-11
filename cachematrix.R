#####################################################################################################################################
# Coursera Project 
# Peer Graded Assignment: R Programming Course Project
# cachematrix.R
# Name: Peng Li
#####################################################################################################################################

### The introduction of the project:
# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than 
# compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
# The assignment is to write a pair of functions that cache the inverse of a matrix.

#####################################################################################################################################
# The R script starts below:
#####################################################################################################################################

### 1. First Function: 
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 if (dim(x)[1]!= dim(x)[2]) {
    message("You Did Not input a Square Matrix") # Return a warning message if the input is not a squre matrix
  }
      inv <- NULL
      set <- function(y) {
        x <<- y
        inv <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      list(set = set,
           get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}

### 2. Sencond Function:
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  # Check if the inverse has been calculated
  if (!is.null(inv)) {
    message("Getting cached data") # Return the message if the inverse matrix has been in the cache
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv      
}
