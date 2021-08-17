## Thank you for grading my assignment. These two functions will
# calculate the inverse of a matrix that can late be cached.

## makeCacheMatrix is a function that can be used to create
# objects containing a list of four function. Later this object
# can cache its inverse.

## define x as an empty matrix and set i to NULL
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  ## the set function. Y is assigned to x and i is set to NULL in
  ## the parent environment
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## this is the getter which can later be used for accessing the
  # data
  get <- function() x
  ## creating a very short function for assigning 'inverse' to
  # i in the parent environment
  setinverse <- function(inverse) i <<- inverse
  ## creating a very short function that can later be used to
  # retrieve data
  getinverse <- function() i
  ## finally, a list of the four functions are created
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## the CacheSolve function calculates the inverse of the object
## that was returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
  ## here data is accessed in x that is ideally the object
  # returned by makeCacheMatrix
  i <- x$getinverse()
  ## checking if the value is already cached. If it has already
  # been computed then that value is returned.
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## If the value has not been computed yet, R accesses the data in
  # in the object, calculates the inverse, assigns it to i in the
  # global and the parent environment and returns it.
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
}
