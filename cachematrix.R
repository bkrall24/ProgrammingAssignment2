## This function caches inverse matrices to avoid repeating operations

## makeCacheMatrix stores the matrix and the inverse of the matrix once calculated

makeCacheMatrix <- function(x = matrix()) {
    
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInv <- function(inv) inverse <<- inv
    getInv <- function() inverse
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)

}


## This function returns the inverse of the matrix

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
    
    inverse <- x$getInv()
    if(!is.null(inverse)) {
        print("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInv(inverse)
    inverse
}
