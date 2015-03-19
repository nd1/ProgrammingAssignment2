## Create a cached matrix. Calculate the inverse of the matrix and store it in cache.

## construct a named list of elements in the environment, for the variable x

makeCacheMatrix <- function(x = matrix()) {
        invrs <- NULL
        set <- function(y) {
                x <<- y
                invrs <<- NULL
        }
        get <- function() x
        setinvrs <- function(solve) invrs <<- solve
        getinvrs <- function() invrs
        list(set = set, get = get,
             setinvrs = setinvrs,
             getinvrs = getinvrs)
}


## calculate the inverse and store as invrs if invrs is null. otherwise, get the cached value of invrs.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invrs <- x$getinvrs()
        if(!is.null(invrs)) {
                message("getting cached data")
                return(invrs)
        }
        data <- x$get()
        invrs <- solve(data, ...)
        x$setinvrs(invrs)
        invrs
}
