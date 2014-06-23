## These 2 functions work in tandem to solve and store the results of
## a matrix inversion operation.

## This function stores the value of an inverted matrix
## it contains functions to get and set the matrix itself
## as well as functions to get and set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    # return stored matrix
    get <- function() x
    
    # store the inversion
    setinverse <- function(inv) i <<- inv
    
    # return the stored value
    getinverse <- function() i
    
    # return a object that is a list of the functions available for use
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function returns the saved inverse if available
## or calculates the inverse and stores the result if not already set

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    
    # if we already have a calculated result, return that
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    # if we don't have a stored value, solve, store, and return
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
