## Creates an object that stores an input matrix and its inverse.
makeCacheMatrix <- function( x = matrix() ) {
        ## Init mtrx as NULL.
        ## mtrx will eventually hold the inverse matrix.
        mtrx <- NULL
        
        ## Assaigns x(in the parent env) the matrix in the input argument.
        ## Reset mtrx to NULL.
        set <- function(aMatrix) {
                x <<- aMatrix
                mtrx <<- NULL
        }
        ## Get matrix.
        get <- function() x
        ## Set inverse of matrix.
        setinverse <- function(solve) mtrx <<- solve
        ## Get inverse of matrix
        getinverse <- function() mtrx
        ## Return list with methods.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
## Argument x is needed in order to find the inverse matrix.
## Return a matrix that is the inverse of 'x'.
cacheSolve <- function(x, ...) {
        mtrx <- x$getinverse()
        ## Check if mtrx is in cache, i.e. is not null. If so, return it.
        if(!is.null(mtrx)) {
                message("data from cache")
                return(mtrx)
        }
        ## Get matrix x.
        data <- x$get()
        ## Invert the matrix.
        mtrx <- solve(data, ...)
        ## Cache the inverse matrix.
        x$setinverse(mtrx)
        ## Return mtrx.
        mtrx
}