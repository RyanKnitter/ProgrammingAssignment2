## These functions allow you to cache the Inverse Matrix so that
## R does not have to recalculate it if the original matrix has
## not changed.

## The makeCacheMatrix function creates a list of four functions.
## The "set" function allows you to edit the cached Matrix values, 
## and it sets the cached Inverse Matrix to NULL.
## The "get" function returns the cached Matrix.
## The "setinvm" function edits the cached Inverse Matrix.
## The "getinvm" function returns the cached Inverse Matrix

makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL
        set <- function(y = matrix()) {
                x <<- y
                invm <<- NULL
        }
        get <- function() x
        setinvm <- function(invm2 = matrix()) invm <<- invm2
        getinvm <- function() invm
        list(set = set, get = get,
             setinvm = setinvm,
             getinvm = getinvm)
}

## This function uses the list of functions created by
## makeCacheMatrix and will do one of two things; It will
## either return the cached Inverse Matrix or it will
## calculate the Invese Matrix if one doesn't exist in
## cache. If it calculates the Inverse Matrix, it then
## caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invm <- x$getinvm()
        if(!is.null(invm)) {
                message("getting cached data")
                return(invm)
        }
        data <- x$get()
        invm <- solve(data, ...)
        x$setinvm(invm)
        invm
}
