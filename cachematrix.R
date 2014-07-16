## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Description: This function creates a special matrix object that can
## cache its inverse
## Inputs: the incoming matrix
## Outputs: None
makeCacheMatrix <- function(x = matrix()) {
  inverseOfMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseOfMatrix <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inverseOfMatrix <<- inv
  getinv <- function() inverseOfMatrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
# Description: This function computes the inverse of the special 
# "matrix" returned by makeCacheMatrix function 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
# Inputs: The actual function that stores and performs operations on the initial matrix
# Outputs: The inverse of the matrix and returning of the cached value
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverseOfMatrix <- x$getinv()
    message("right before is not null")
    if(!is.null(inverseOfMatrix)) {
        message("getting cached data")
        return(inverseOfMatrix)
    } else {
      message("the inverse is null")
    }
    data <- x$get()
    inverseOfMatrix <- solve(data,...)
    x$setinv(inverseOfMatrix)
    inverseOfMatrix
}
