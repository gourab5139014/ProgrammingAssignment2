# This function allows us to return cached value of the inverse of a matrix
# Contains the following functions:
# * setMatrix      set the value of a matrix
# * getMatrix      get the value of a matrix
# * cacheInverse   get the cahced value (inverse of the matrix)
# * getInverse     get the cahced value (inverse of the matrix)
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL #flush cache on new matrix assignment
    }
    get <- function() x
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv , getinv = getinv)
}


## This function returns the inverse of a matrix. It retreives value from cache is possible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) { #fetch from cache if possible
        message("getting data from cache")
        return(inv)
    }
    data <- x$get()
    inv <- solve(x) #solve when data is not available in cache
    x$setinv(inv) #put new result in cache
    inv
}
