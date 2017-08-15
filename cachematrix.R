## The following funstions are used to cache the inverse of a matrix in order
## to save computation cycles.

## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    sol <- NULL
    set <- function(y) {
        x <<- y
        sol <<- NULL
    }
    get <- function() x
    setsol <- function(solve) sol <<- solve
    getsol <- function() sol
    list(set = set, get = get,
         setsol = setsol,
         getsol = getsol)
}


## The following function calculates the inverse of a matrix but first, the
## functions checks if the inverse has already been calculated. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    sol <- x$getsol()
    if(!is.null(sol)) {
        message("getting cached data")
        return(sol)
    }
    data <- x$get()
    sol <- solve(data, ...)
    x$setsol(sol)
    sol
}