## Introduces a new matrix object that will cache the inverse
## of the matrix to improve the speed of getting the inverse.
## The new matrix object is implemented as a list.

## Create the list that represents the new matrix object that
## caches its inverse.
## Pass in the initial matrix.
makeCacheMatrix <- function(x = matrix()) {
    # Variable to store the inverse of the matrix
    matrix_inverse <- NULL
    # Function to set the matrix, used after the matrix object
    # is initialized.  Note that setting a new matrix value requires
    # the cached inverse of the matrix to be reset.
    set <- function(y) {
        x <<- y
        matrix_inverse <<- NULL
    }
    # Function to return the matrix
    get <- function() {
        x
    }
    # Function to save the cached matrix inverse
    saveinverse <- function(inverse) {
        matrix_inverse <<- inverse
    }
    #Function to retrieve the cached matrix inverse
    getinverse <- function() {
        matrix_inverse
    }
    # Return the list representing the matrix object to the caller
    list(set = set, get = get, saveinverse = saveinverse, getinverse = getinverse)
}


## Function to call when solving for the inverse of a new matrix object.
## If the inverse has already been calculated by a previous call to cacheSolve
## that will be returned.  Otherwise the inverse will be calculated, saved
## as the cached value, and returned to the caller.
cacheSolve <- function(x, ...) {
    # First try to get cached copy of the inverse    
    inv <- x$getinverse()
    if (!is.null(inv)) {
        # If we have a valid cached inverse, return that
        message("cached inverse found")
        return(inv)
    }
    # Otherwise we have not yet calculated this inverse, solve for it now
    inv <- solve(x$get(), ...)
    # Save the calculated inverse in the cache
    x$saveinverse(inv)
    # Return the calculated inverse
    inv
}
