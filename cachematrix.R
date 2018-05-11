## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## (there are also alternatives to matrix inversion that we will not discuss here).
## Your assignment is to write a pair of functions that cache the inverse of a matrix.


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## holds the cached value or NULL if nothing is cached
  ## initially nothing is cached so set it to NULL
  
  invert <- NULL
  ## store the matrix
  
  set <- function(y) {
    x <<- y
    invert <<- NULL
  }
  ## return the stored matrix
  
  get <- function() x
  ## cache the given argument 
  
  setinverse <- function(inverse) invert <<- inverse
  getinverse <- function() invert
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##I f the inverse has already been calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## restore cached argument from above
  
  invert <- x$getinverse()
  if(!is.null(invert)) {
    message("getting cached data")
    return(invert)
  }
  
  data <- x$get()
  invert <- inverse(data, ...)
  x$setinverse(invert)
  invert
  ## print result
}
