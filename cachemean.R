main<-function(){
  makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean#surely we don't want to use 'mean' as a variable name?!
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
  }
  
  cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
  }
  
  v1 = c(4,2,6);
  makeV1 = makeVector(v1);
  meanV1 = cachemean(makeV1);
  v2 = c(8,4,2);
  makeV2 = makeVector(v2);# this puts the vector
  return(list(makeV1 = makeV1, meanV1=meanV1));
  
}

