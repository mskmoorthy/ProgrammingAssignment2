## Creates a Cache Matrix Modeled after the sample program defines member functions

makeCacheMatrix <- function(x = matrix()) {

inVerse <- NULL
  set <- function(y) {
    x <<- y
    inVerse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inVerse <<- inverse
  getinverse <- function() inVerse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

##
## Test Input
## x = rbind(c(1, -1/2), c(-3/4, 1))
##m = makeCacheMatrix(x)
##m$get()
##m$getinverse()
###cacheSolve(m)

## Cache Solving

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inVerse <- x$getinverse()
  if(!is.null(inVerse)) {
    message("getting cached data.")
    return(inVerse)
  }
  data <- x$get()
  inVerse <- solve(data)
  x$setinverse(inVerse)
  inVerse

}
