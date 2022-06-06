## Put comments here that give an overall description of what your
## functions do
## This function caches the inverse of a given matrix and returns.
## If the contents of the matrix does not change, the cached
## value of the inverse will be looked up and returned instead of 
## being recomputed.

## Write a short comment describing this function
## This first function creates a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y = matrix()) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
## This second function computes the inverse of the matrix returned
## from the first function and checks if the inverse has been calculated
## assuming the matrix does not change and retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$getinv()
        inv
        ## Return a matrix that is the inverse of 'x'
}
