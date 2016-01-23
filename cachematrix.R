##  These functions create a special 'matrix' object that is actually a list
##  of functions that contains a matrix and can store the inverse of that matrix.

##  This function creates the list object containing the matrix.  It
##  has 4 functions that can be called to set the internal matrix
##  to a matrix passed as an argument, retrieve the internal matrix, 
##  set the inverse matrix to a passed matrix, or retrieve the inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(z) m <<- z
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
##  This function accepts the special 'matrix' object that is actually a list
##  containing a matrix as an argument.  It then attempts to retrieve the inverse
##  matrix, if the inverse matrix does not exist then it calculcates the 
##  inverse using the solve function and passes it back to the special 
##  matrix object where it is stored.  It then returns the inverse matrix.
cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}