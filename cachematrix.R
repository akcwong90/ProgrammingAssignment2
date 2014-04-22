## Put comments here that give an overall description of what your
## functions do

## Given a numeric vector, constructs a square matrix.  
## Assumes a dimensions are a squarable value.  Also provides methods 
## to get and set the inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() matrix(x, sqrt(length(x)), sqrt(length(x)))
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


##  Caches the inverse of matrix and provides its values.
##  If not, performs a solve to compute it, and caches the result.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
