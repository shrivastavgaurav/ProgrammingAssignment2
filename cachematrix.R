## The cachematrix.R function has a matrix argument and builds
## a set of functions and returns them to a list in the parent environment
## A sample matrix: m1<-matrix(c(1/2,-1/4,-1,3/4),nrow=2,ncol=2)
## Execute: aMatrix<-cachematrix(m1)
## Execute: aMatrix$get() - will return m1
## Execute: aMatrix$getinv() - will return NULL

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
    
  }
  get <- function() x
  setinverse <- function(tbd) m <<- tbd
  getinverse <- function() m
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## The cacheSolve will either solve for the inv of the matrix if m is NULLL
## or retrieve the inv from the object passed in as the argument
## Execute: cacheSolve(aMatrix) - will return the inv of m1
## Execute: cacheSolve(aMatrix) - will return the inv of m1 from
## the data already calculated and print out that it is getting the cached data
## Execute: aMatrix$getinv() - will return the inv of m1 already

cacheSolve <- function(x, ...) {
        
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
