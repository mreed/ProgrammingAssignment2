## makeCacheMatrix creates a matrix capable of caching its inverse matrix
## cacheSolve returns the inverse of a matrix, using the cached version if it exists

## makeCacheMatrix createa a matrix that can cache its inverse.
## by calling setinv you will cache the inverse matrix
## call getinv so retrieve the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function takes a makeCacheMatrix and returns is inverse.
## It will use a cached version of the inverse matrix if it exists.
cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)){
            message("getting inverse matrix from cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinv(m)
        m
}
