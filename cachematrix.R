## Collaborative functions that take an invertible matrix
## and allow for caching and management of it and its inverse.
## Recommended use:
##     initialize with cm <- makeCacheMatrix(x)
##     use cm$get() to examine matrix contents
##     use cm$set(y) to reset matrix value and inverse
##     use cacheSolve(cm) function to get and/or set inverse;
##     do not use cm$getinverse() or cm$setinverse(inv) directly

## Given a matrix assumed to be invertible, produces a placeholder (m)
## for its inverse and returns a list of management functions

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) m <<- inv
    getinverse <- function() m
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## Given x as a list product of makeCacheMatrix, checks for an existing
## inverse to return and solves for it if none exists yet.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
