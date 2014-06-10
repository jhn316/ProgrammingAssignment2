# the makeCacheMatrix function creates a "matrix" object. 
# When called, the set methods sets the matrix given by the user
# when called, the get methods gets the matrix that was given by the user
# the setinv method calculates the inverse of the matrix given by the user and caches it
# the getinv method returns the inverse by calculation or from the cache.


## The makeCacheMatrix function creates matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

#This function computes the inverse of the matrix returned by the makeCacheMatrix function. 
#If the inverse has been cached, it retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}


