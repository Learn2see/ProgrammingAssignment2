## This is the first of two function for assigning a value to an object that resides
## in a different environment than the current environment.  It creates a "special vector".

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
                
        }
        get <- function() x
        setmean <- function(mean) m <<- mean()
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}


## This second function calculates the mean of the vector created in the first function.
## It first checks to see if the mean has been previously calculated.  If so it pulls the value
## from the cache.  If not it calculates the mean and set the result in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$mean(m)
        m
}