## Makes use of lexical scoping rules of R to cache the computation of  
## inverse of a matrix; computes the inverse if the matrix undergoes change

## makeCacheMatrix: creates the special matrix 'x' and its inverse 'inv'
## constructs an environment with 'x', 'inv' and four manufactured functions - 
## set(), get(), set_inv() and get_inv()

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        set_inv <- function(inverse) inv <<- inverse
        get_inv <- function() inv
        
        list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)        
}

## Returns the inverse of a matrix contained in x
## Attempts to return the cached inverse first; 
## Computes the inverse if cache is empty, by invoking solve()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get_inv()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        dat <- x$get()
        inv <- solve(dat, ...)
        x$set_inv(inv)
        inv
}
