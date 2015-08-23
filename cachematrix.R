## This file contains functions to define a custom object that is able to cache a matrix and its inverse.


## This function returns a list of functions to get and set the value of the argument matrix x, in addition to
## get and set the value of its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function returns the inverse of the argument matrix x and caches its value. If the inverse has already
## been computed before, the cached value is returned.

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


## Usage Example:
##
## > m <- matrix(1:4, 2, 2)
## > cm <- makeCacheMatrix(m)
## > cm$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > cacheSolve(cm)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(cm)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > 
