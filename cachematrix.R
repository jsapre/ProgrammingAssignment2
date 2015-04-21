## This source file is part of submission of second programming assignment
## for coursera course rprog-013 as submitted by jsapre@hotmail.com
## The functions in this source file offer a wrapper around matrix
## to calculate its inverse such that it is cached.


## For given invertible matrix x this function
## gives back a wrapper list object that can be used
## with cacheSolve to get cached inverse of the matrix x

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function takes the special object created by makeCacheMatrix
## checks if inverse exists as cached matrix else calculates it and returns the same

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}
