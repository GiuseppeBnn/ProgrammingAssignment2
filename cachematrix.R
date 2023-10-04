## Put comments here that give an overall description of what your
## functions do

##makeCacheMatrix creates a "matrix" object that can cache its inverse for the input matrix.
##The inverse is calculated by the function ginv() from MASS library.
##the cachesolve() should retrieve the inverse from the cache if the inverse is already present.

library(MASS) ##this is the library that contains the ginv() function

makeCacheMatrix <- function(x = matrix()) {
        ##getter and setter functions for the matrix
        i <- NULL
        setInverse <- function(inverse) {
                i <<- inverse
        }
        getInverse <- function() {
                i
        }
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() {
                x
        }
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        i <- x$getInverse()     
        if(!is.null(i)) {            ##if is present in cache, return it
                message("getting cached data")
                return(i)            
        }
        data <- x$get()
        i <- solve(data, ...)       ##if not present in cache, calculate it and store it in cache
        x$setInverse(i)
        i
}

