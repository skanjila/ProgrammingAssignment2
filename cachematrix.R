## Put comments here that give an overall description of what your
## functions do
## Author: Saikat Kanjilal
## Class: Coursera r programming
## Program: HW2

## This function creates a matrix containing a function
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix
## Inputs: the incoming matrix
## Outputs: None
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# This function returns the inverse of the matrix by checking
# to see if its in the cache and performing the computation
# to retrieve the value only if needed
# Inputs: The actual function that stores and performs operations on the initial matrix
# Outputs: The inverse of the matrix and returning of the cached value
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invMatrix <- x$getinverse()
    message("right before is not null check")
    if(!is.null(invMatrix)) {
        message("getting matrixed cached data")
        return(invMatrix)
    } else {
      message("the inverse of the matrix is null therefore doing the computation")
    }
    #retrieve the data
    data <- x$get()
    # call the solve function
    invMatrix <- solve(data)
    #set the inverse
    x$setinverse(invMatrix)
    #we're done, return the inverse
    invMatrix
}
