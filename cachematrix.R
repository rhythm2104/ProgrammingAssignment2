##                  Programming Assignment 2: Lexical Scoping 

## makeCacheMatrix makes an object capable of catching its inverse,
## cacheSolve returns the inverse of the matrix passed to makeCacheMatrix
## for this code the matrix is always considered to be invertible

## makeCacheMatrix makes a special matrix object that can catch its inverse
## It also contains four functions for storing and retrieving the values

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL     # initializing an empty variable for storing the inverse
  
  ## function for storing the value of the matrix to be inverted
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  
  ## function for retrieving the value of the stored matrix
  get <- function()x
  
  ## function for storing the value of the calculated inverse
  setInverse <- function(inverse) j <<- inverse
  
  ##function for retrieving the current value of the inverse
  getInverse <- function() j
  
  #list of functions
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)

}


## cacheSolve returns the inverse of 'x', either from the cached memory or
## after calculating the inverse if the cache is empty. In the latter case it
## also stores the calculated value in the cache. The function takes an argument
## of type makeCacheMatrix so a matrix needs to be passed to makeCacheMatrix() 
## before running this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  j <- x$getInverse()   #retrieving the currently cached value
  
  ## if a currently cached value exits it is returned and no further
  ## calculations are performed
  
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  
  #otherwise
  
  mat <- x$get()       # gets the matrix to be inverted
  j <- solve(mat,...)  # computes the inverse of the matrix
  x$setInverse(j)      # stores the value in the cache
  j                    # returns the value of the inverse
}
