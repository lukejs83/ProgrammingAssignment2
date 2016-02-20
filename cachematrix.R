## The functions makeCacheMatrix() and cacheSolve() allow for caching the inverse of 
## a matrix so that matrice only need be inverted once. 

#makeCacheMatrix() returns a special matrix object that can cache its inverse.
#
# Args:
#       M: an invertible matrix to be stored in the special matrix object  
#
# Functions:
#        set(y): Sets matrix M to arg y. y is a matrix.
#        get(): returns matrix M
#        setinverse(yinverse): sets inverse Minverse to arg yinverse
#        getinverse(): returns inverse Minverse
#        
# Returns:
#       A special matrix object that consists of a list of functions used to
#       manipulate the matrix.

makeCacheMatrix <- function(M = matrix()) {
        
        Minverse <- NULL

        set <- function(y) {
                M <<- y
                Minverse <<- NULL
        }
        
        get <- function() M

        setinverse <- function(yinverse){
                
                Minverse <<- yinverse
        }
        
        getinverse <- function() Minverse
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}

#cacheSolve() returns the inverse of a special matrix object. 
#
# Args:
#       x: a special matrix object to be inverted  
#
# Returns:
#       The inverse of special matrix x.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        Minverse <- x$getinverse()
        
        if(!is.null(Minverse)) {
                message("getting cached inverse")
                return(Minverse)
        }
        data <- x$get()
        Minverse <- solve(data, ...)
        message("computing inverse")
        x$setinverse(Minverse)
        Minverse
        
}
