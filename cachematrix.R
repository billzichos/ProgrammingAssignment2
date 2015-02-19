##  Matrix inversion is usually a costly computation and there may be some 
##  benefit to caching the inverse of a matrix rather than compute it 
##  repeatedly. The functions below calculate the inverse of a matrix after 
##  confirming it is not already in cache.

## The makeCacheMatrix function creates a special "matrix" object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        ## Set the value of the matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ## Get the value of the matrix
        get <- function() x
        
        ## Set the value of the inverse
        setinverse <- function(solve) i <<- solve
        
        ## Get the value of the inverse
        getinverse <- function() i
        
        ## Create the "special" vector that is the list of functions.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


##  The cachSolve function computes the inverse of the special "matrix" returned
##  by makeCacheMatrix above. If the inverse has already been calculated (and 
##  the matrix has not changed), then the cachesolve should retrieve the inverse
##  from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        
        ## Check to see if the inverse has already been created
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
