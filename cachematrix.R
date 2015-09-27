## Programming Assignment 2

## These R functions are able to cache the potentially time-consuming 
## computation of inverting a matrix.  If the content of a matrix
## is not changing it may make sense to cache the inverse rather than
## recomputing it when we need it again.

## Note:  For this assignment, we can assume that the matrix supplied 
## is always invertible.

## makeCacheMatrix creates a special vector whose items are functions:
## 'set' - sets the value of a matrix
## 'get' - returns the value of a matrix
## 'setInverse' - sets the inverse of a matrix
## 'getInverse' - gets the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
     v <- NULL
     set <- function(y) {
          x <<- y
          V <<- NULL
     }
     get <- function() x
     setInverse <- function(solve) v <<- solve
     getInverse <- function() v
     list(set = set, get = get,
          setInverse = setInverse, 
          getInverse = getInverse)
}

## The function 'cachesolve' returns the inverse of matrix x.  
## It checks to see if the inverse has already been done and if
## it has, it returns the inverse from cache.  If the inverse is not
## in cache it creates it, stores it in cache using 'setInverse' 
## and returns it.

cacheSolve <- function(x, ...) {
     ## Get the inverse of matrix x from cache
     v <- x$getInverse()
     
     ## If the inverse is not null, it was previously done, return it
     if(!is.null(v)) {
          message("getting cached data")
          return(v)
     }
     
     ## if the inverse is null, get the matrix, create the inverse,
     ## store it in cache using 'setInverse', and return it
     data <- x$get()
     v <- solve(data, ...)
     x$setInverse(v)
     v
}
