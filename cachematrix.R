## The following are a couple of function to calculate the inverse of a Matrix and chace it.

## makeCacheMatrix takes a matrix as input. 
## For now we assume the the given matrix is inversable. It returns a list of four functions
##   - set() 
##   - get(),         
##   - setInverse() 
##   - getInverse()

makeCacheMatrix <- function(x = matrix()) {
    inverseMX = NULL
    
    set <- function(mx) {
        x <<- mx
        inverseMX <<- NULL
    }
    
    get <- function()
        x
    
    setInverse <- function(inv) {
        inverseMX <<- inv
    }
    
    getInverse <- function()
        inverseMX
    
    return(list(
        set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse
    ))
}


## cacheSolve creates the inverse of a given matrix and cahces it. 
## If the Inverse is already present in the cache, it returns the cached inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inversedMatrix <- x$getInverse()
    if (!is.null(inversedMatrix)) {
        message("Getting cached data...")
        return(inversedMatrix)
    }
    
    origMatrix <- x$get()
    inversedMatrix <- solve(origMatrix, ...)
    x$setInverse(inversedMatrix)
    inversedMatrix
}
