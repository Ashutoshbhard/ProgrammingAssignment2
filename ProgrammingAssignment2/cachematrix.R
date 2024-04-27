## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # Initialize the cache
  cache <- NULL
  
  # Setter function to set the matrix
  setMatrix <- function(matrix) {
    x <<- matrix
    cache <<- NULL  # Clear the cache when the matrix is updated
  }
  
  # Getter function to retrieve the matrix
  getMatrix <- function() {
    x
  }
  
  # Setter function to set the inverse of the matrix
  setInverse <- function(inverse) {
    cache <<- inverse
  }
  
  # Getter function to retrieve the inverse of the matrix
  getInverse <- function() {
    cache
  }
  
  # Return a list of functions
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the special matrix returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  # Retrieve the cached inverse if available
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("Getting cached result.")
    return(inverse)
  }
  
  # If the inverse is not cached, compute it
  matrix <- x$getMatrix()
  inverse <- solve(matrix, ...)
  
  # Cache the inverse
  x$setInverse(inverse)
  
  # Return the computed inverse
  inverse
}
