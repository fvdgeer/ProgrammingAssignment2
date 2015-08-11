## This is my solution for the programming task of week 2.

## This function wraps a matrix with an object to cache the inverse calculation of the matrix.
## It works in conjunction with the cacheSolve() function.
## Example:
##    cm <- makeCacheMatrix(c(5, 2, 0, 3), 2, 2)
##    cacheSolve(cm)
## The inverse of the matrix will be calculated.
## Another call to cacheSolve(cm) will return the result taken from the cache.
makeCacheMatrix <- function(theMatrix = matrix()) {
    inv <- NULL
    set <- function(aMatrix) {
        theMatrix <<- aMatrix
        inv <<- NULL
    }
    get <- function() theMatrix
    setinv <- function(theInverse) inv <<- theInverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
    
}


## This function determines the inverse of a matrix that has been wrapped with makeCacheMatrix().
## The inverse will only be calculated once and will then be cached for later usage.
cacheSolve <- function (cacheMatrix, ...) {
    inv <- cacheMatrix$getinv()
    if (is.null(inv)) {
        m <- cacheMatrix$get()
        inv <- solve(m, ...)
        cacheMatrix$setinv(inv)
        message("computed result")
    }
    else
    {
        message("cached result")
    }
    inv
}
