## A set of functions to create the inverse of a matrix and
## cache the resulting matrix

## Usage:
## ##create a 2x2 matrix 'a' with data elements 1:4
## a<-matrix(data=1:4, nrow=2, ncol=2)
##
## ##initialize the makeCacheMatrix object using the matrix 'a'
## ma<-makeCacheMatrix(a)
##
## ## calculate the inverse matrix by calling cachesolve()
## sa <-cacheSolve(ma)
##
## ## now call the same function again, the output should now indicate
## ## that we're calling the cached version of the inverse matrix
## sa <-cacheSolve(ma)


## makeCacheMatrix creates a function that is a list with the methods
## of setting and getting the cached inverse matrix as well as 
## calculating the inverse matrix if it has not yet been calculated

makeCacheMatrix <- function(x = matrix()) {
  ## if the function is called without a method then return NULL
  im <- NULL
  
  ## a function set that assigns the inverse matrix to 'x' 
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  
  ## a function that gets the inverse matrix stored in 'x'
  ## if the inverse matrix has not yet been assigned to 'x'
  ## then the result will return a NULL indicating
  ## that the inverse matrix needs to be calculated first
  get <- function() {
    x
  }
  
  ## assigns the input solve to variable 'im'
  setsolve <- function(solve) {
    im <<- solve
  }
  
  ## retrieve the content of variable 'im'
  getsolve <- function() {
    im
  }
  
  ## create a list with the methods for the makeCacheMatrix function
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
  
}


## This is the actual solver function

cacheSolve <- function(x, ...) {
  ## Return a matrix 'im' that is the inverse of 'x'
  
  ## retrieve the cached inverse matrix via the 'getsolve' method
  m <- x$getsolve()
  
  ## if 'm' is not NULL then we retrieved the cached inverse matrix
  ## and can exit out of the function
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  message("calculating inverse matrix")

  ## we now assign the input matrix 'x' to variable 'data'
  data <- x$get()
  
  ## calculate the inverse matrix to 'data' and assign to 's'
  s <- solve(data, ...)
  
  ## now cache the resulting inverse matrix
  x$setsolve(s)
  
  # return the calculated inverse matrix 's'
  return(s)
}
