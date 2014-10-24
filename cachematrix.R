## Programming Assignment 2 for R Programming
## There are two functions in this file
## 1. makeCacheMatrix: this function creates a special
##    "matrix" object that can cache its inverse.
## 2. cacheSolve: this function computes the inverse of 
##    the special "matrix" returned by makeCacheMatrix above.
##    If the inverse has already been calculated (and the matrix
##    has not changed), then the cachesolve should retrieve the 
##    inverse from the cache.

## makeCacheMatrix creates a special "matrix" object which is
## in fact a list containing four functions, similar to makeVector
## function in the example. I will write more comments to help you
## understand the four functions in the list.
## Thanks to the dynamic feature of R, the code is quite similar to
## the original example.
makeCacheMatrix <- function(x = matrix()) {
    ## i is the matrix object which is the inverse of 'x'
    ## Possibly not a good variable name, but I would just be lazy
    i <- NULL
    ## set function sets the the original matrix 'x'
    ## Remember to set i to NULL, because 'x' has been changed
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    ## get function gives you the original matrix 'x'
    get <- function() x
    ## setinv function sets the inverse of 'x'
    ## or it caches the inverse of 'x'
    setinv <- function(inv) i <<- inv
    ## getinv function gets the cached inverse of 'x'
    getinv <- function() i
    ## Return the special matrix object
    list(set = set, get = get,
         setinv = setinv,
         getinv = getmean)
}

## cacheSolve is the main function, which takes advantage of makeCacheMatrix
## to calculate and cache the inverse of the given matrix 'x'
## Pay attention that 'x' here is not a matrix, but a special object (list)
## returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
    ## Calls the getinv function to get the cached inverse
    i <- x$getinv()
    ## Checks whether the inverse is not NULL, then we get the cached inverse
    ## and return
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ## Gets the real 'x' matrix
    data <- x$get()
    ## Calculates the inverse of the matrix 'x' using the solve function
    i <- solve(data, ...)
    ## returns the inverse
    i
}
