## The function caches the inverse of matrix.
## when the inverse is called again, it checkes, if it hasn't been changed,
## it returned the old/cached value.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## This function checks if inverse of the matrix has been called before
## and didn't change, it returns its cached value.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}


##Unit test code

A <- matrix(c(2,4,5,3,7,8,3,6,2), nrow=3,ncol=3)
ACached = makeCacheMatrix(A)
AInversed = cacheSolve(ACached)

#The output
#> AInversed = cacheSolve(ACached)
# getting cached data
# > AInversed = cacheSolve(ACached)
# getting cached data

##The following should return an identify matrix
A %*% AInversed