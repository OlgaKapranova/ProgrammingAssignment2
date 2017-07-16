## Programming Assignment 2: Lexical Scoping
## Author: OlgaK
## Date: 16/07/2017
## Version 1.0

## Write function "makeCacheMatrix" which creates a list of functions and
## sets their environment accordingly for a given input matrix (numeric):
## 1st element: set() - re-sets a local variable x within environment of 
## makeCacheMatrix function which stores your input matrix
## 2nd element: get() - restores a value of local variable x from environment
## of makeCacheMatrix function (if any! otherwise restores NULL)
## 3rd element: setinv() - sets a local variable inv within environment of 
## makeCacheMatrix function which stores inverse of your input matrix written
## in another local variable x (only stores, but does not calculate!)
## 4th element: getinv() - restores a value of local variable inv from 
## environment of makeCacheMatrix function (if any! otherwise restores NULL)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(invers) inv <<- invers
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Write function "cacheSolve" which either restores local variable inv from
## environment of function "makeCacheMatrix" applied before using "cacheSolve"
## function OR calculates it from local variable x from environment of
## function "makeCacheMatrix" and writes back to local variable inv from
## environment of function "makeCacheMatrix"

## Assumption: Input matrix is always invertible!

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
