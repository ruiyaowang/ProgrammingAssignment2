## Put comments here that give an overall description of what your
## functions do
##Below are two functions that are used to create a special object that stores a matrix and caches its reverse

## Write a short comment describing this function
## makeCacheMatrix creates a special "matrix", which is really a list containing a function to set and get matrix, set and get matrix reverse
makeCacheMatrix <- function(x = matrix()) {
  rv <- NULL
  set <- function(y) {
    x <<- y
    rv <<- NULL
  }
  get <- function() x
  setreverse <- function(reverse) rv <<- reverse
  getreverse <- function() rv
  list(set = set, get = get,
       setreverse = setreverse,
       getreverse = getreverse)
}


## Write a short comment describing this function
## The following function calculates the reverse of the special "matrix" created with the above function. However, it first checks to see if the reverse has already been calculated. If so, it gets the reverse from the cache and skips the computation. Otherwise, it calculates the reverse of the data and sets the value of the reverse in the cache via the setreverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  rv <- x$getreverse()
  if(!is.null(rv)) {
    message("getting cached data")
    return(rv)
  }
  data <- x$get()
  rv <- solve(data, ...)
  x$setreverse(rv)
  rv
}
