## makeCacheMatrix, gets as input an invertiable matrix
## returns new object which encapsulates the matrix with
## function to calculate efficiently the inverse when called via
## cacheSolve function
makeCacheMatrix <- function(x = matrix()) {
    ## internal value to cache the inverse of x
    inv <- NULL
    
    ## set other matrix to the encapsuled matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## get the original matrix
    get <- function() x
    
    ## cache the calculated inverse
    setinv <- function(inv_mat) inv <<- inv_mat
    
    ## get the inverse matrix
    getinv <- function() inv
    
    # returns a list of function to manipulate the encapsuled matrix
    list (set=set, get=get, setinv=setinv, getinv=getinv)
}


## Works with makeCacheMatrix to calculate and cache the inverse of 
## a matrix
cacheSolve <- function(x, ...) {
    ## get the cached matrix
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## get here only if inverse wasn't calculated
    data <- x$get()
    ## calculate inverse and cache the value for next usages
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
