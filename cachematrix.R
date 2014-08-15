## Collaborative functions that take an invertible matrix
## and allow for caching and management of it and its inverse.
## Recommended use:
##     initialize with cm <- makeCacheMatrix(x)
##     use cm$get() to retrieve matrix contents
##     use cm$set(y) to reset matrix value and inverse
##     use cacheSolve(cm) function to get and/or set inverse of cm.
##
##     DO NOT use cm$getinverse() or cm$setinverse(inv) directly, as
##         they are intended for use only by cacheSolve

## Given a matrix assumed to be invertible, produces a placeholder (cmi)
## for its inverse and returns a list of management functions
## Among those functions, only get and set should be used directly.

makeCacheMatrix <- function(x = matrix()) {
    cmi <- NULL
    set <- function(y) {
        x <<- y
        cmi <<- NULL
    }
    get <- function() x
    setinverse <- function(nv) cmi <<- nv
    getinverse <- function() cmi
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## Given x as a list product of makeCacheMatrix, checks for an calculated
## inverse and solves for it if none is stored yet, returning the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        nv <- x$getinverse()
        if(!is.null(nv)) {
            message("getting cached data")
            return(nv)
        }
        data <- x$get()
        nv <- solve(data, ...)
        x$setinverse(nv)
        nv
}
