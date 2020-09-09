## Taking the inverse of a matrix is very computationally expensive.
## In the case where we have to compute the inverse multiple times 
## of the same matrix, we will cache inverse so if needed again we
##  do not have to compute but only fetch it from the cache.

## Creates a special "matrix", which is really a list containing functions to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## set the value of the mean
## Assume the matrix is invertible. 

makeCacheMatrix <- function(x = matrix()) {
        ## placeholder for a future value
        inv <- NULL
        ## defines a functions to set y to a new matrix y and resets the inv to NULL
        set <- function(y) {
                ## double arrow operator can modify variables in the parent level
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setinverse <- function(inverse) {inv <<- inverse}
        getinverse <- function() {inv}
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'
## First checks if inverse has already been calculated
## If yes, it gets the inverse from the cache 
## Otherwise, it computes the inverse of the matrix using the solve command

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting the cached inverse")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
