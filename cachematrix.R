## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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



## Write a short comment describing this function

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
