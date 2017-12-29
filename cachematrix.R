
## These 2 functions compute the inverse of a square matrix. The inverse of the square matrix is cached.

## The first function is makeCacheMatrix().
## The second function is cacheSolve().

## cacheSolve() is required for makeCacheMatrix() to be complete.


makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
         x <<- y
         i <<- NULL
   }
   get <- function() x
   setinverse <- function(solve) i <<- solve
   getinverse <- function() i
   list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)      
}
## This function creates a special matrix object that can cache its inverse.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
   if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     data <- x$get()
     i <- solve(data, ...)
     x$setinverse(i)
     i      
}
## This function returns a matrix that is the inverse of the square matrix 'x'.
