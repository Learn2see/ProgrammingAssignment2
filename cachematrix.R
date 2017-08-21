## This is the first of two function for assigning a value to an object that resides
## in a different environment than the current environment.  It creates a "special vector".

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
                
        }

      # Setup the function and returns matix  
        get <- function() 
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This second function calculates the inverse matrix of the first function.
## It first checks to see if the matrix has been previously calculated.  If so it pulls the value
## from the cache.  If not it calculates the inverse and set the result in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getmean(x,...)
        inv <- x$getinverse()
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}