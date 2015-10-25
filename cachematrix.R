##Functions to cache the inverse of a matrix to avoid computing
## the inverse of a matrix repeatedly as this is time consuming


## makeCacheMatrix creates a special matrix object that cache its inverse 
## (only square invertible matrix as "solve" function only calculates
## the inverse of a square invertible matrix)

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve calculates the inverse of the matrix created by makeCacheMatrix.
## It retrieves it from the cache if it is already calculated
## Otherwise, it calculates the inverse using "solve" function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 
    m <- x$getinverse()
   
    ## evaluates if it is already calculated in cache at first place
    ## if so, it returns the inverse of "x" without wasting time 
    ## calculating it from scrach
    
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    
    ## if it cannot retrieve the inverse from the cache
    ## it calculates it and stores it in cache with setinverse
    
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
  
}
