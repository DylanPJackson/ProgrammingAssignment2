# filename 
#       cachematrix.R 
#
# description
#       A pair of functions that can cache the inverse of a matrix 
#
# author
#       Dylan P. Jackson

# Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y 
        i <<- NULL
    }
    get <- function() x
    set_inverse <- function(inv) i <<- inv
    get_inverse <- function() i
    list(set = set, get = get, set_inverse = set_inverse, 
            get_inverse = get_inverse)
}


# Computes inverse of special matrix returned from makeCacheMatrix.
# If the inverse has already been calculated and the matrix has not changed,
# cacheSolve retrieves the inverse from the cache
cacheSolve <- function(x) {
    i <- x$get_inverse()
    if(!is.null(i)){
        message("Getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(x)
    x$set_inverse(i)
    i
}
