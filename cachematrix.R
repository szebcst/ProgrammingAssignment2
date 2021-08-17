## Thank you for grading my assignment. These two functions will
# calculate the inverse of a matrix that can late be cached.

## makeCacheMatrix is a function that can be used to create
# objects containing a list of four function. Later this object
# can cache its inverse.

## defining x as an empty matrix object and setting i to NULL
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  ## The setter. y is assigned to x and i is set to NULL in
  # the parent environment. This is important in case i already has
  # a value in the parent environment.
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## The getter can later be used for accessing the data
  get <- function() x
  ## creating a very short function for assigning 'inverse' to
  # i in the parent environment
  setinverse <- function(inverse) i <<- inverse
  ## creating a very short function that can later be used to
  # retrieve data
  getinverse <- function() i
  ## finally, creating a list of the four functions. naming them
  # will help to use them in the next function more easily.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## the CacheSolve function calculates the inverse of the object
## that was returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
  ## here, data is accessed in x (that is ideally the object
  # returned by makeCacheMatrix)
  i <- x$getinverse()
  ## checking if the value is already cached. If it has already
  # been computed then "!is.null(i))" is true and its value is
  # returned.
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
