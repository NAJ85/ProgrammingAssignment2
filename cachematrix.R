## The first function takes a matrix and returns a list with setter and getter
## functions to be used in the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL ##setting the inverse to NULL
    
    ## create the method to set the value of the matrix and the inverse to NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x ## return the matrix
    setinverse <- function(inverse) inverse <<- inverse ## set the inverse of the matrix
    getinverse <- function() inverse ## return the inverse of the matrix
    ## return a list of functions that can be accessed by the cacheSolve function
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function retruns the inverse of a matrix 'x'
## If the inverse has already been cached it returns the cahced result and
## a message saying it was cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse() ## retrive the inverse created by makeCacheMatrix
    
    ## checks to see the inverse has already been calculated. If so returns the
    ## calculated value and a messgae indicating the cached data is being returned
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse) ## returns the inverse and exits the function
    }
    data <- x$get() ## set the data variable to the matrix passed from makeCacheMatrix
    inverse <- solve(data, ...) ## sets inverse to the inverse of the matrix
    x$setinverse(inverse) ## sets the inverse of the matrix
    inverse ## returns the inverse of the matrix
}
