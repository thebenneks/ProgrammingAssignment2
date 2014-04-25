## This module contains two functions makeCacheMatrix and cacheSolve used to 
## calculate the inverse of a square matrix and caching of the inverse for subsequent calls
## To use the functions 
##    1. call makeCacheMatrix(<matrix>) with the respective matrix
##       returns a special matrix with setter & getter methods
##
##    2. call cacheSolve(<special matrix>) with the returned matrix from step 1.
##       this will return the inverse of the original matrix
##       the inverse gets calculated only for the first call
##       all subsequent calls just return the cached inverse


############################################################################
##
## cacheSolve       returns the inverse of the original matrix 
##                  the inverse will only calculated the first time
##                  all subsequent calls just return the cached inverse
############################################################################
cacheSolve <- function(x, ...) {
  ## get the inverse matrix value from the special matrix x 
  invX <- x$getInvM()
  ## check if the return value isn't null
  if(!is.null(invX)) {
    ## return the cached inverse
    message("getting cached data")
    return(invX)
  }
  ## else get the original matrix 
  data <- x$get()
  ## calculated the inverse of it
  invX <- solve(data, ...)
  ## store the result for subsequent calls
  x$setInvM(invX)
  ## return the inverse matrix
  invX
}


############################################################################
##
## makeCacheMatrix  that returns a new object that contains 
##                  the original matrix and setter and getter methods to 
##                  retrieve the orignal matrix and the inverse of it
############################################################################
makeCacheMatrix <- function(x = matrix()) {
  ## store the parameter for later calls
  X <- x
  ## initialize the invX to NULL
  invX <- NULL
  ## the setter function for the original matrix
  set <- function(x) {
    ## store it in the internal var X
    X <<- x
    ## set the invX var to NULL to trigger recalculation of the inverse
    invX <<- NULL
  }
  ## getter function for the original matrix
  get <- function() X
  ## setter function for the inverse matrix 
  setInvM <- function(invM) invX <<- invM
  ## getter function for the inverse matrix
  getInvM <- function() invX
  ## return the setter & getter functions as list 
  list(set = set, get = get,
       setInvM = setInvM,
       getInvM = getInvM)
}
