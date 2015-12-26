## A pair of functions that cache the inverse of a matrix

## Creates a list of functions that will:
## Set Matrix
## Get Matrix
## Set its inverse
## Get its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    ## Change the matrix stored in the main function
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    ## Return the matrix stored in the main function
    get <- function() x
    ## Store value of inverse as variable 'i'
    setinverse <- function(inverse) i <<- inverse
    ## Return value of inverse of matrix
    getinverse <- function() i
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}    

## Computes the inverse of the matrix created by makeCacheMatrix

cacheSolve <- function(x, ...){
    ## return the inverse of matrix 'x'
    i <- x$getinverse()
    ## Check if 'i' already has a value, return it if so
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ## Otherwise, get matrix from first function, store as 'data'
    data <- x$get()
    ## Use 'solve' function to calculate the inverse of the matrix
    i <- solve(data, ...)
    ## store the result in the cache in the main function
    x$setinverse(i)
    return(i)
}