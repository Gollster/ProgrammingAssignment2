## Name:cachematrix.R
## This R file contains two funtions
## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##                If the inverse has already been calculated (and the matrix has not changed), then cacheSolve
##                should retrieve the inverse from the cache.



## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    #here we set the inverse of the input matrix
    setinverse <- function(solve) m <<- solve
    #and here we go get it
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
    
    m <- x$getinverse()
    #if there is a cached value for the inverse we return that
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    #if this is not the  case we calculate and return
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
