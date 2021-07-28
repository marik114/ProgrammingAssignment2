## makeCacheMatrix & cacheSolve to return inverse of matrix from cache

## makeCacheMatrix creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    #set new data
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    
    #get current data
    get <- function() x
    
    #setinv
    setinv <- function(inv) m <<- inv
    
    #getinv
    getinv <- function() m
    
    #return list of function handles
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve calculates inverse of special matrix, or cache value
## if previously calculated

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if (!is.null(m)) {
        message("Getting cached value")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinv(m)
    m
}
