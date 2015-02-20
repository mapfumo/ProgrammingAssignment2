## Author: Antony Mapfumo
## 20 February 2015

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Two functions that cache a matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ## set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## let's get the matrix
  get <- function() x
  
  ## set the inverse
  invMatSet <- function(inverse) inv <<- inverse
  ## get the inverse
  invMatGet <- function() inv
  
  ## Return the matrix 
  list(set = set, get = get, invMatSet = invMatSet, invMatGet = invMatGet)
}


## Write a short comment describing this function
## cacheSolve returns the inverse of a matrix
## If the inverse is already calculated it returns the cached inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$invMatGet()
  
  # If the inverse is already calculated:: return inverse
  if (!is.null(inv)) {
    message("Getting cached inverse matrix")
    return(inv)
  }
  
  # The inverse isn't yet calculated:: calculate inverse
  data <- x$get()
  inv <- solve(data, ...)
  
  # Cache the inverse
  x$invMatSet(inv)
  
  ## Return the inverse matrix
  inv
}
