# create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# compute the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
  # retrieve the inverse from the cache if available
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # if the inverse is not in the cache, calculate it and cache it
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
