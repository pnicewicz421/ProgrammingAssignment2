# The makeCacheMatrix and cacheSolve functions cache the inverse of a matrix. 
# The matrix provided must be an invertible matrix (otherwise, an error will be
# produced)

# makeCacheMatrix: This function creates a special "matrix" object that can 
#                  cache its inverse.
#                  The special "matrix" object is a list containing a function
#                  to:
#                  1. set the value of the matrix
#                  2. get the value of the matrix
#                  3. set the value of the inverse of the matrix
#                  4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

# cacheSolve: This function computes the inverse of the special "matrix" 
#             returned by makeCacheMatrix 
#             above. If the inverse has already been calculated (and the matrix
#             has not changed), then the cachesolve should retrieve the inverse
#             from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
