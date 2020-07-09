## A set of functions to 1. compute the inverse of a matrix and 2. create a special object to cache the inverse of the 
## matrix, to save on computation if repeated multiple times.


## A function that takes a matrix as an argument, checks whether the inverse of that matrix has been computed already
# and cached. The function then defines 4 functions to "get" and "set" data values and then assigns each function 
## as an element within a list.

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        set <- function(s) {
                x <<- s
                i <<- NULL
        }
        
        get <- function() x
        setinv <- function(solve) i <<- solve
        getinv <- function() i
        list(set = set, get = get, setinv = setinv, getinv = getinv)

}

## A function that takes the list created using makeCacheMatrix as an argument, checks whether a cache exists, if
## it does, it returns the cached computed inverse of the matrix, otherwise computes the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        i <- x$getinv()
        if(!is.null(i)){
                message("Returning data that is already cached")
                return(i)
        }
        
        data <- x$get()
        i <- solve(data,...)
        x$setinv(i)
        i
}
