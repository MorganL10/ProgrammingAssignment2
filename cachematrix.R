##two functions to cache the inverse of a matrix
##stage a matrix to be cached

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ##set the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ##get the matrix, create the function to do the solve
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## will solve for the matrix if it has not already been cached
## will return the cached value if the matrix has been solved
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  ##check if matrix has already been solved
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##calculate if the matrix has not been solved
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
