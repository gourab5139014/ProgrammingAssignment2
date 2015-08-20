## Put comments here that give an overall description of what your
## functions do

## This function caches the value representing inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(i) inv <<- i
    getinv <- function() inv
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting data from cache")
        return(inv)
    }
    data <- x$get()
    inv <- solve(x)
    x$setinv(inv)
    inv
}
