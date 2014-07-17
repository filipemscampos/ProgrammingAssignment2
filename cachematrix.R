## Matrix inversion is usually a costly computation and their may be some benefit to caching the 
##  inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix 
##  inversion that we will not discuss here). Your assignment is to write a pair of functions that 
##  cache the inverse of a matrix.



## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    
    set <- function(y) { 
        x <<- y
        m <<- NULL ## if matrix is updated, inverse will have to be calculated again.
    }
    
    get <- function() x
    
    setInverse <- function(inverse) m <<- inverse
    
    getInverse <- function() m
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated, then the cachesolve 
##  should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    m <- x$getInverse()
    
    if(!is.null(m)) {
        message("getting cached inverse matrix data")
        return(m)
    }
    
    data <- x$get()
    
    m <- solve(data) ## calculate inverse
    x$setInverse(m)
    m
}


