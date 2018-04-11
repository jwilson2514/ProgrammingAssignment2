##This pair of functions provides a mechanism for caching the inverse of a matrix
## to avoid the costly computation of calculating the inverse matrix each time it is needed.

## makeCacheMatrix creates a list of functions to be leveraged by cacheSolve below. 

makeCacheMatrix <- function(x = matrix()) {
        inverseM <- NULL
        set <- function(y) {
                x <<- y
                inverseM <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverseM <<- solve
        getinverse <- function() inverseM
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve checks whether the inverse of a matrix has already been calculated. If not it proceeds with the inverse calculation.

cacheSolve <- function(x, ...) {
        inverseM <- x$getinverse() 
        if(!is.null(inverseM)) {                ##check if the inverse has already been calculated.
                message("getting cached data")
                return(inverseM)
        }
        data <- x$get()                         ## otherwise calculate the inverse of the matrix
        inverseM <- solve(data, ...)
        x$setinverse(inverseM)
        inverseM
        
        ## Return a matrix that is the inverse of 'x'
}
