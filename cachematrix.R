


## makeCacheMatrix is a function that returns an object created from input argument (matrix).
## The object also has another corresponding object (NULL by default) and functions which give full read/write access 
## to both input argument (matrix) and corresponding object.
## When argument object is changed corresponding object is set to NULL.

makeCacheMatrix <- function(x = matrix()) {
        invCM <- NULL
        setCM <- function(y) {
                x <<- y
                invCM <<- NULL
        }
        getCM <- function() x
        setIM <- function(matrx) invCM  <<- matrx
        getIM <- function() invCM 
        list(set = setCM , get = getCM ,
             setIM = setIM ,
             getIM = getIM)
}



## cacheSolve function will need an object created by  makeCacheMatrix() function as an argument.
## To call makeCacheMatrix(M) you need an argument M to be a square matrix
## When called for the first time for certain argument it will calculate and return inverse matrix of the M and cache it 
## All further calles for the same object will result in return of inverse matrix that was cached earlier.


cacheSolve <- function(x, ...) {
        m <- x$getIM()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setIM(m)
        m
}