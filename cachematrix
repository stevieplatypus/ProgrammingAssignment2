## Caching the Inverse of a Matrix
## This set of functions allows for the caching of the inverse of matrices 
## rather than computing them repeatedly.
## Example
## matrix <- matrix(1:4, 2, 2)
## cachematrix <- makeCacheMatrix(matrix)
## cacheSolve(cachematrix)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

## This function creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set,
             get = get,
             setinve = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special matrix. If the inverse is
## already computed, the cached inverse is returned instead.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
