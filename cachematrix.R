## These two functions cache and solve the inverse of a matrix. The first
## function, makeCacheMatrix, creates a list of functions useful for solving
## the inverse. The second function, cacheSolve, will solve the matrix from
## the list created from makeCacheMatrix

## This function will cache the matrix and return a list containing functions
## which will be used when solving for the inverse. These functions are:
## set: Sets the matrix
## get: Grabs the matrix (stored in this list)
## setinverse: Sets the inverse function
## getinverse: Gets the inverse function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <-- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function will solve the invert matrix cache and return the inverse of
## the matrix. It takes the output from makeCacheMatrix and returns the inverse.
## If the inverse does not exist, it will apply the inversion function; if it
## does exist, then the function will load the inverse from the cache and thus
## save memory if, say, this is used more than once on the same matrix.
## solve(x) == cacheSolve(makeCacheMatrix(x)) --> does the same thing

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}