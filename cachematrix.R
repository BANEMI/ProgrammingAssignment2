##  makeCacheMatrix and cacheSolve are a pair of functions that cache and compute the 
##  inverse of a matrix.These functions make the process more efficinet.




##  makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y) {
    x <<- y;
    invx <<- NULL;
  }
  get <- function() x;
  setinv <- function(inv) invx <<- inv;
  getinv <- function() invx;
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

##   cacheSolve: This function computes the inverse of the special "matrix" returned 
##   by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
##   then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  invx <- x$getinv()
  if(!is.null(invx)) {
    message("Getting cached data")
    return(invx)
  }
  data <- x$get()
  invx <- solve(data, ...)
  x$setinv(inverse)
  invx
}