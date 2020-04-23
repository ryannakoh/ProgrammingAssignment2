## Caching the inverse of a matrix!

## This function creates a matrix that can cache its inverse.
## - set function allows you to reset the value of the matrix
## - get function allows you to retrieve the matrix
## - getinv function allows you to retrieve the inverse after 
##   using the cacheSolve function below to calculate the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <<- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(solve) i <<- solve
        getinv <- function() i
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function returns the inverse of the matrix returned by the 
## 'makeCacheMatrix' above.
## If the inverse has already been calculated, it will retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
