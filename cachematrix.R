## This file contains functions to calculate and cache inverse of a matrix.
##
## Simplest usage scenario:
##
## 1. Create inversible square matrix
## m <- matrix(1:4, 2, 2)
## 2. Make cachable matrix
## cm <- makeCacheMatrix(m)
## 3. Calculate the inverse of the matrix. Note that we pass cachable matrix.
## cacheSolve(cm)
## 4. All next executions get use of cached version of the rusult.
## cacheSolve(cm)

## Makes the special "matrix" object that can cache its inverse.
## Arguments:
## x - matrix to initialize cachable object with.
## Returns: cachable "matrix" object
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL

    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x

    setInverse <- function(i) inverse <<- i

    getInverse <- function() inverse
    
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Computes the inverse matrix of the cachable "matrix" object.
## If the inverse has already been calculated for the given "matrix" object
## function will just retrieve the value from object's cache.
## Arguments:
## x - cachable "matrix" object returned by makeCacheMatrix function.
## Returns: inverse matrix
cacheSolve <- function(x) {
    inverse <- x$getInverse()
    
    if (!is.null(inverse)) {
        message('Use inverse from cache.')
        return(inverse)
    }
    
    data <- x$get()
    inverse <- solve(data)
    x$setInverse(inverse)
    inverse
}
