## This module contains 2 functions, the makeCaheMatrix function and the cacheSolve() 
## function. These functions are meant to be used in conjuctions with one another in order
## to same time when re-computing the inverse of a matrix

## the function makeCacheMatrix() creates a special "matrix" that can cache its inverse
## this special "matrix" is actually a list containing 4 functions. 
## This special matrix is meant to be used in conjuctions with the cacheSolve() function, 
## which computes the inverse of matrix 'x'
##  
## Args:
## x: an invertable matrix
##
## Returns:
## A list containing 4 functions (see function descriptions within makeCacheMatrix()):
## set()
## get()
## setInverse()
## getInverse()

makeCacheMatrix <- function(x = matrix()) {
  
  ## define 'i' to hold the inverse of the matrix 'x', it is initialized to "NULL" until
  ## the first call to the getinverse() function
  i <- NULL
  
  ## The following 4 function definitions rely on R lexical scoping rules. 
  ## As the set(), get(), setInverse(), getInverse() functions are defined 
  ## within the makeCacheMatrix() function, R first attempts 
  ## to determine the value of 'x' and/or 'i' within the functions themselves, 
  ## then within the function where they were defined, the makeCacheMatrix() function
  
  set <- function(y) {
    ## this function to used set or change the value of the underlying matrix 'x'
    ## each time set() is invoked, it is assumed that the underlying matrix
    ## changed and the inverse, 'i' is reset to "NUll" 
    
    ## Args: 
    ## y: an invertable matrix
    
    ## as 'x' and 'i' are in the environment of makeCacheMatrix(),
    ## the <<- operator is required 
    x <<- y
    i <<- NULL
  }
  
  ## this function simply retuns the underlying matrix 'x'
  get <- function() x
  
  ## this function to assigns the inverse of the matrix 'x' as 'i' is in the 
  ## environment of makeCacheMatrix(), hence the <<- operator is required     
  setInverse <- function(inverse) i <<- inverse
  
  ## this function simply returns the inverse of the matrix 'x'  
  getInverse <- function() i

  ## Returns a list of functions defined above
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## the fuction cacheSolve() computes the inverse of the special "matrix" returned by makeCacheMatrix()
## the invered matrix will be cached so that future calls to invert the matrix 
## via the cacheSolve() function will first check to see if the matrix has already 
## been inverted via the solve() function, if so the cached result will be returned 
## and the solve() computation will be skipped
##  
## Args:
## x: A special Matrix, that is actually a list of functions created 
##    via the makeCacheMatrix() function, 
##
## Returns:
## the inverse of the underlying matrix maintained by 'x'

cacheSolve <- function(x, ...) {
  
  ## get the inverse of the underlying matrix
  i <- x$getInverse()

  ## if the resulting matrix 'i' is not 'NULL', the inverse of the underlying matrix
  ## has already been computed, so simply return the cached result and skip the
  ## costly solve() function
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  
  ## as the inverse of the underlying matrix has not yet been computed, get the 
  ## underlying matrix
  data <- x$get()
  
  ## compute the inverse of the underlying matrix
  i <- solve(data, ...)

  ## set/cache the inverse so that future calls to the cacheSolve() function
  ## can take advantage of the cached inverse
  x$setInverse(i)

  ## return the inverse 'i' of the underlying matrix maintained by 'x'
  i
}

