## We will write a pair of functions that cache the inverse of a matrix to save 
## computation costs

## This function is creating the special "matrix" object by setting the value 
## of matrix, getting the value of the matrix, setting the inverse of matrix, 
## and getting the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function takes the special ?matrix" and computes the inverse from 
## makeCacheMatrix. The function retrieves the inverse from the cache if it has
## already been calculated.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
