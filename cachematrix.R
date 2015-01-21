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


## This function cacheSolve calculates the inverse of a matrix which has been created in the above
## function makeCacheMatrix. This function checks to see if the matrix has already been calculated.
## If it has it uses that value from the cache. If it hasn't it calcultes the inverse of the matrix.

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of x
    inv.mat <- x$getinverse()          # get cached inverse value
    if(!is.null(inv.mat)) {   		   # check if the cache is not empty, if it is not then return it.
        message("getting cached data") # helpful message
        return(inv.mat)				   # return the inverse matrix
    }
    message("calculating new inverse")
    matrix <- x$get()        # get the matrix
    inv.mat <- solve(matrix) # get the inverse of the matrix
    x$setinverse(inv.mat)    # cache this result
    inv.mat                  # output the inverse of the matrix
}
