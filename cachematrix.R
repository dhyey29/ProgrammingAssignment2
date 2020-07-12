## The makeCacheMatrix() function creates a matrix, which is really a list
## with functions to set the values in matrix, get the values of matrix, set 
## the inverse of matrix, get the inverse of matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(mat)
    {
        x <<- mat
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## First, we get the inverse and then check if it is set. If inv is NULL, then
## the inverse of matrix x is calculated and set to inv and the inverse is 
## printed, else the inv value is cached using getinv() and returned

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv))
    {
        message("Using cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
