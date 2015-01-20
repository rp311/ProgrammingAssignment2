## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv.mat <- NULL
    set <- function(y) {
        x <<- y
        inv.mat <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv.mat <<- solve
    getinverse <- function() inv.mat
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function nmn,n,n

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of x
    inv.mat <- x$getinverse() # get cached inverse value
    if(!is.null(inv.mat)) {   # check if the cache is not empty, then return inv.mat
        message("getting cached data")
        return(inv.mat)
    }
    message("calculating new inverse")
    matrix <- x$get()        # get the matrix
    inv.mat <- solve(matrix) # get the inverse of the matrix
    x$setinverse(inv.mat)    # cache this result
    inv.mat                  # output the inverse of the matrix
}
