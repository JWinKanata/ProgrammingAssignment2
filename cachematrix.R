## Purpose: Caching the inverse of a matrix rather than computing it
## repeatedly to avoid costly computation (when possible) and
## speed up the operation. Provided are two functions,first one
## caches the inverse of a matrix, and the other one computes the
## inverse. 
## We assume that the matrix supplied is always invertible!
## Calling sequence is "z <- cacheSolve(makeCacheMatrix(x))" ...tested, it works. 
## This function creates a special "matrix" object that will cache
## its inverse.
makeCacheMatrix <- function(x = matrix()) {
    ## set the value of the matrix
    im <- NULL
    set <- function(y) {
        x <<- y      ## note the use of superassignment statements
        im <<- NULL  ## that makes 'x' and 'im' accessible outside 
                     ## this environment to other function
    }
    ## get the value of the matrix
    get <- function() x
    ## set the value of the inverse matrix
    setinverse <- function(solve) im <<- solve
    ## get the value of the inverse matrix
    getinverse <- function() im
    ## Put these four functions in a 'list' to make them
    ## callable from the other,'cacheSolve', function
    list(set = set,get = get,
         setinverse = setinverse,getinverse = getinverse)
}
cacheSolve <- function(x, ...) {
    ## Returns a matrix that is the inverse of 'x'. If the inverse
    ## has already been calculated and the matrix has not changed,
    ## then the inverse is retrieved from the cache.
    im <- x$getinverse()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    data <- x$get()
    im <- solve(data,...)        ## compute the inverse of the matrix
    x$setinverse(im)
    im
}
