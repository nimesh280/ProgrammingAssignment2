## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and therefore it is better to 
## cache the inverse of a matrix rather than computing it repeatedly.
## Below are two functions which are used to create an object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invmat <<- inverse
  getInverse <- function() invmat
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated and the 
## matrix has not changed, then it should retrieve the inverse from the cache.
## otherwise it will again calculate the inverse and store it in the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invmat <- x$getInverse()
  if (!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  mat <- x$get()
  invmat <- solve(mat, ...)
  x$setInverse(invmat)
  invmat
}
