## These functions calculate the inverse of a square matrix, and include it in the cache to make sure one doesn't have to recompute in case the same matrix is resubmitted for computation

## This function makeCacheMatrix creates a special "vector", which is really a list containing a function to
## sets the value of the vector
## gets the value of the vector
## sets the value of the inverse
## gets the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function sets the inverse in the cache - however, the function first checks whether the cache already has the vector and the inverse

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
