## Function to calculate the inverse of a matrix and save the inverse to the cache for future use.

## This function creates a special "matrix" allowing the inverse to be saved in the cache. 
## Used to set and get value of matrix.
## Used to set and get value of the inverse.

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


## Checks if the inverse is already calculated and saved in the cache. 
## If not set in the cache it calculates the inverse and saves the value in the cache.

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
