## Two functions here, first one creates a cache'able matrix,
## second one calculates its inverse

## This function creates a cache'able matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # Data setters / getters
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  # Returning a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Ths function creates an inverse of a matrix and caches it
cacheSolve <- function(x, ...) {
  # Checking if inverse was cached
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # Calculating the inverse
  data <- x$get()
  m <- solve(data)
  # Caching data
  x$setinverse(m)
  m
}
