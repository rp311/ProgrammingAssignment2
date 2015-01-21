## Programming Assignment 2
##
## The two functions below makeCacheMatrix and cacheSolve are used to calculate the inverse of a matrix
## defined as "x". If the inverse matrix has been calculated and cached before, that result will be
## returned i.e. the cached result. If not a new inverse matrix will be calculated. 

## Part1: This function makeCacheMatrix creates a matrix that can cache the inverse of the matrix. 
## It returns a list of four fuctions.

makeCacheMatrix <- function(x = matrix()) {
    inv.mat <- NULL       # set inverse matrix to NUL
    set <- function(y) {
        x <<- y           # set the value
        inv.mat <<- NULL  # clear the cache with NULL
    }
    get <- function() x                             # function to get the value of the matrix
    setinverse <- function(solve) inv.mat <<- solve # function to set the inverse of the matrix
    getinverse <- function() inv.mat                # function to get the inverse of the matrix
    list(set = set, get = get,                      # generate a list of functions
         setinverse = setinverse,
         getinverse = getinverse)
}


## Part2: This function cacheSolve calculates the inverse of a matrix which has been created in the
## above function makeCacheMatrix. This function checks to see if the matrix has already been
## calculated. If it has, it uses that value from the cache. If it hasn't it calcultes the inverse of
## the matrix.

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of x
    inv.mat <- x$getinverse()          # get cached inverse value
    if(!is.null(inv.mat)) {            # check if the cache is not empty, if it is not then return it.
        message("getting cached data") # helpful message
        return(inv.mat)                # return the inverse matrix
    }
    message("calculating new inverse")
    matrix <- x$get()        # get the matrix
    inv.mat <- solve(matrix) # get the inverse of the matrix
    x$setinverse(inv.mat)    # cache this result
    inv.mat                  # output the inverse of the matrix
}
