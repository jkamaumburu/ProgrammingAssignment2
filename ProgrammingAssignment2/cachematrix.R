## This is week 3 assignment 2 for R Programming course
## Assignment to reduce the computational cost of matrix inversion by caching the inverse rather than computing it repeatedly.


## The Function below will be used to create a special matix object than can be used to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    matrixInverse <- NULL
    set <- function(y) {
            x <<- y
            matrixInverse <<- NULL
    }
    get <- function() x
    
    setInverse <- function(inverse) matrixInverse <<- inverse
    getInverse <- function() matrixInverse
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## This function below will be used to Compute the inverse of the special matrix created using the makeCaheMatrix function
## For already calculated inverse and unchanged matrix, the inverse is retrieved from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    matrixInverse <- x$getInverse()
    if(!is.null(matrixInverse)) {
        message("getting cached data")
        return(matrixInverse)
    }
    mat <- x$get()
    matrixInverse <- solve(mat, ...)
    x$setInverse(matrixInverse)
    matrixInverse
}
