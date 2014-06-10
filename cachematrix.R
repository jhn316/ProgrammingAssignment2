# the makeCacheMatrix function creates a "matrix" object. 
# When called, the set method sets the matrix given by the user
# when called, the get method gets the matrix that was given by the user
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



#### Optional #####
# The code below demonstrates the use the above functions
# using a sample 3x3 matrix

# syntax to use the above functions-like classes and methods

x <- makeCacheMatrix() # x is a "matrix" object

y <- matrix(c(1,0,5,2,1,6,3,5,0),3,3) # y is the input matrix which is invertible

x$set(y) # assign y as the matrix object

cacheSolve(x) # return the inverse of the input matrix from either the cache or by calculation