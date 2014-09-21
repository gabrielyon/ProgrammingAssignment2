##21 September 2014
##Author: Gabriel Lyon

##This pair of functions allows the user to perform the inversion of a
##matrix. If this matrix has already been processed, it will return the
##cached inverse value.

## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    ## define the set function to store the matrix x
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## define the get function to return the matrix x
    get <- function() x
    ## define the setInverse function to store the inverse of the matrix
    setInverse <- function(inverse) inv <<- inverse
    ## define the getInverse function to get the stored inverse value
    getInverse <- function() inv
    ## return a list of above-defined functions
    list(set = set, get = get, setInverse = setInverse, 
         getInverse = getInverse)
}


##This function takes the matrix object created in makeCacheMatrix
##and returns the inverse of matrix 'x' that that function works with.
##If this calculation has already been performed, it will return the 
##cached value (and alert the user that it is doing so)

cacheSolve <- function(x, ...) {
    ##Check for a cached value
    inverse <- x$getInverse()
    if(!is.null(inverse)){
        message("Getting cached data")
        return(inverse)
    }
    ##Calculate inverse for non-cached value
    matrix <- x$get()
    inverse <- solve(matrix)
    x$setInverse(inverse)
    inverse
    
}
