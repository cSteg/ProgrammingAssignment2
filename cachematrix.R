##These functions first create a matrix, stores it along with other user defined functions in 
##a cache using makeCachMatrix, and then calls the matrix and executes the functions in cacheSolve.

## Write a short comment describing this function
##This function defines a matrix and a series of functions to be used with that matrix. Using magic.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    } 
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
##This function calls the matrix defined in makeCasheMatrix, and executes the previously defined functions. With magic.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}