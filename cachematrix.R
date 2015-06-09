## Since it can be computationally intensive to calculate the inverse of a matrix, it
## may be wise to store the inverse of a matrix once it has been calculated in order to
## make it more efficient to retrieve the inverse subsequently. 

## makeCacheMatrix creates an enhanced matrix which can store its inverse.
## The matrix can be accessed via getters and setters as can its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve returns the inverse of enhanced matrix 'x'. If the inverse has previously
## been computed, the cached inverse is returned. Otherwise the inverse is calculated
## for the first time and stored.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- inverse(data, ...)
  x$setinverse(m)
  m
}
