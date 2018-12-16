## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
        i <- x$getinvert() #attempts to retrieve mean from makeVector() object
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get() #gets the vector for the input object
        i <- solve(data, ...) #calculates the mean, only place where it is executed
        x$setinvert(i) #set the mean in the input object
        i #prints mean object
}
