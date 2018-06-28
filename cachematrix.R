## Title: cache matrix
## Author: Ben Cowey
## Date: 28/06/2018

# Description: This code creates 2 functions the first caches matrixs and
#               their means the second caclulates and caches the inverse if 
#               not already cached.

## Function to Cache Matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) x
        getinverse <- function() m
        list( set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}


## Function to check whether the inverse has been calculated and if not
## calculate and cache the inverse

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
