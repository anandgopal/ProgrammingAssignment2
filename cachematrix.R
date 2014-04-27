## R Programming | Week 3 | Peer Assessment Assignment
## Caching the Inverse of a Matrix
## 27 April, 2014
## These functions can be used to cache the inverse of a matrix, instead of having to
## compute it each time we need the inverse. 

## The special matrix (which is really a list) stores both the matrix as well as 
## caches the inverse. The function 'makeCacheMatrix' achieves this structure

## The function 'cacheSolve' looks to see if an inverse is already cached. If so, it 
## retrieves the inverse from the cache. If there is no cached inverse, it computes it


## This function creates the special "matrix", which is really a list of functions. It 
## contains functions to set the value of the matrix, get the value of the matrix, 
## set the value of the inverse and get the value of the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
    Inverse <- NULL
    
    set <- function(y) {
        x <<- y
        Inverse <<- NULL
    }
    
    get <- function() x
    setInverse <- function(Inv) Inverse <<- Inv
    getInverse <- function() Inverse
    
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## This function can be used to find out the inverse of the matrix created by the 
## function above. It is, however, more efficient than the traditional 'solve' function
## If the inverse is already cached in the special matrix, it retrieves it from the 
## cache. If the cache is empty, then it computes the inverse, displays it and also 
## stores the inverse back in the cache using the setInverse function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    Inverse <- x$getInverse()
   
    if(!is.null(Inverse)) {
       message("getting cached Inverse Data")
       return(Inverse)
    }
    
    data <- x$get()
    Inverse <- solve(data, ...)
    x$setInverse(Inverse)
    
    Inverse
}
