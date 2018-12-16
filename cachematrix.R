## Functions take a matrix as an argument and returns the inverse of the matrix.
## If the inverse has been previously computed, it will return the cached result

## Takes matrix as an argument and returns a list containing the matrix and functions to get
## and set the matrix and its inverse. If the inverse has been calculated, the list will
## include the calculated inverse

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        set <- function(y) {
                x <<- y
                invert <<- NULL
        }
        get <- function() x
        setinvert <- function(invert) i <<- invert
        getinvert <- function() i
        list(set = set, get = get,
             setinvert = setinvert,
             getinvert = getinvert)
}


## Takes makeCacheMatrix object as an argument. If the inverse has been previously
## calculated, will return the cached inverse. If it has not, it will calculate the
## inverse and store it in the makeCacheMatrix object

cacheSolve <- function(x, ...) {
        
        i <- x$getinvert() 
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get() 
        i <- solve(data, ...)
        x$setinvert(i)
        i
}
