## makeCacheMatrix: This function creates a special "matrix" object 
##                  that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" 
##             returned by makeCacheMatrix above. If the inverse has 
##             already been calculated (and the matrix has not changed), 
##             then the cacheSolve will retrieve the inverse from the cache.


## Returns the special matrix 
## which can cache the inverse of matrix
## This is actually a list containing functions to
##    1. Set the value of matrix
##    2. Get the value of matrix
##    3. Set the value of the inverse matrix
##    4. Get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialization of inverse matrix with NULL
  i <- NULL
  set <- function(y){
    ## Sets the value of x and reinitializes
    ## the inverse matrix to NULL
    x <<- y
    i <<- NULL
  }
  
  ## Returns the matrix x
  get <- function(){
    x
  } 
  setInverse <- function(inverse) {
    ## Sets the inverse matrix
    i <<- inverse
  }
  getInverse <- function(){
    ## Returns the cached inverse matrix
    i
  }
  
  ## Returns the special matrix
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
  
}


## Returns the inverse of special matrix
## created by makeCacheMatrix method. If
## the inverse has already been calculated
## then the cached inverse is retrieved and 
## returned.

cacheSolve <- function(x, ...) {
      ## Returns a matrix that is the inverse of 'x'
      i <- x$getInverse()
      if(!is.null(i)){
        ## The inverse is already calculated so cached
        ## data is returned
        message("getting cached data")
        return(i)
      }
      
      ## Inverse is to be caculated
      
      ## Geting matrix whose inverse is to be
      ## calculated
      data <- x$get()
      i <- solve(data)     ## Calculate the inverse using solve method
      x$setInverse(i)      ## Set the result in cache
      i
}
