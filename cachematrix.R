##cacheSolve requires an argument that is returned by makeCacheMatrix() in order
##to retrieve the inverse from the cached value that is stored in the 
##makeCacheMatrix() object's environment. cacheSolve computes the inverse of
##the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed)
##, then the cachesolve should retrieve the inverse from the cache.


## Return a matrix that is the inverse of 'x'
## the function attempts to retrieve an inverse from the matrix
## object passed in as the argument. First, it calls the getinverse()
## function on the input object
    
makeCacheMatrix <- function(x = matrix()) {
    invrs <- NULL
    set <- function(y) {
        x <- y
        invrs <- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invrs <- inverse
    getinverse <- function() invrs
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## if the result of !is.null(n1) is FALSE, cacheSolve() gets the matrix
## from the input object, calculates the solve(), uses the setinverse()
## function on the input object to set the inverse in the input object,
## and then returns the value of the inverse to the parent environment
## by printing the inverse object

cacheSolve <- function(x, ...) {
    invrs <= x$getinverse()
    if(!is.null(invrs)) {
        message("getting cached data")
        return(invrs)
    }
    mat <- x$get()
    invrs <- solve(mat, ...)
    x$setinverse(invrs)
    invrs
}