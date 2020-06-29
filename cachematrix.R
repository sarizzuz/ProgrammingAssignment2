# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. 
# The following functions will cache the inverse of a matrix.


# This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

        # initialize the inverse of matrix x that is to be computed
        invx <- NULL
        
        # sets the the value of the matrix x
        set <- function(y) {
            x <<- y
            invx <<- NULL
        }
        
        # gets the value of the matrix x
        get <- function() x
        
        # sets the inverse of the matrix (i.e. invx)
        setinv <- function(inverseX) invx <<- inverseX
        
        # gets the inverse of the matrix x, (i.e. invx)
        getinv <- function() invx
        
        # returns a list of the functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix. If the inverse already exists, then the function will 
# simply retrieve the data from the cache.

cacheSolve <- function(x, ...) {
        
        # getting the inverse matrix from the cache
        invx <- x$getinv()
        
        # if inverse matrix exists in cache, it is returned as the result
        if (!is.null(invx)) {
            message(('getting cached data'))
            return(invx)
        }
        
        # assigns the matrix x to the variable "data"
        data <- x$get()
        
        # calculates the inverse of the matrix
        inv <- solve(data)
        
        # sets the value of the calculated inverse in the cache
        x$setinv(invx)
        
        # returns the inverse of matrix x
        invx
}
