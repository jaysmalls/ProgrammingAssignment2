## 1. makeCacheMatrix creates a matrix and returns a list of caching functions
## 2. cacheSolve returns a cached or calculated inverse of a matrix


## makeCacheMatrix:
## Creates a matrix and returns a list of the functions setMatrix, getMatrix,
## getInverse and setInverse
makeCacheMatrix <- function(matrix = matrix()) {

    ## Initialises "inverse" (the cache)
    inverse <- NULL

    ## setValue sets "matrix" in the parent makeCacheMatrix environment to the
    ## value of it's parameter "newMatrix" and reinitialises "inverse" (the
    ## cache)
    setMatrix <- function(newMatrix) {
        matrix <<- newMatrix
        inverse <<- NULL
    }

    ## getValue returns the value of "matrix"
    getMatrix <- function() matrix

    ## setInverse sets the value of "inverse" (the cache) in the makeCahceMatrix
    ## function to the value of it's parameter "inverse"
    setInverse <- function(inverse) inverse <<- inverse

    ## getInverse returns the value of "inverse"
    getInverse <- function() inverse

    ## Creates a list of the functions above as the return value of the
    ## makeCacheMatrix function
    list(setVal = setMatrix,
         getVal = getMatrix,
         setInv = setInverse,
         getInv = getInverse)    
}


## cacheSolve:
## Returns a cached inverse of "the "matrix" if it exsits, else calculates
## and returns the inverse of the matrix
cacheSolve <- function(matrix, ...) {

    ## Sets the local "inverse" value to the value returned by the getInverse
    ## function (the inverse value in the makeCacheMatrix function)
    inverse <- matrix$getInv()

    ## Tests if "the local "inverse" is not null and, therefore, has been
    ## populated with the cached value.  If so "inverse" is returned stopping
    ## the cacheSolve function
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }

    ## If the local "inverse" is null, then the inverse needs to be calculated.
    ## "data" is set to the value of the matrix by calling the getValue function
    data <- matrix$getVal()
    
    ## The inverse of the matrix is then calculated with the solve function
    inverse <- solve(data, ...)
    
    ## The makeCacheMatrix's "inverse" value (the cache) is then set with this
    ## function's "inverse" value
    matrix$setInv(inverse)
    
    ## Return the "inverse" value
    inverse
}
