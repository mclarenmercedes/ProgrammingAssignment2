## makeCacheMatrix : creates a wrapper around the matrix with 4 helper functions.
## 1. get : get the matrix "x".
## 2. set : set the matrix "x".
## 3. getinverse: get the inverse of the matrix "x".
## 4. setinverse: set the inverse of the matrix "x".

makeCacheMatrix <- function(m = matrix()) {
  dims <- dim(m)
  stopifnot (length(dims) == 2 && dims[1] == dims[2] && dims[1] > 0)
  cache <- NULL
  set <- function(y) {
    m <<- y
    cache <<- NULL
  }
  get <- function() m
  setinverse <- function(inverse) cache <<- inverse
  getinverse <- function() cache
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns the inverse of a matrix.
## If the inverse of a matrix has already been computed, then, the cached inverse is returned.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
