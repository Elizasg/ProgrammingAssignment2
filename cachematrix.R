## The functions below create a special object that stores a matrix and caches its inverse. Therefore, the inverse won't have to be calculated again when we need it; it will be looked up in the cache instead. 

## the makeCacheMatrix function creates a special object (m) that stores a matrix and and caches its inverse.


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        m <<- solve(x)
        setinverse <- function(inverse) m <<- solve(x)
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse, getinverse = getinverse)
}

## The cacheSolve function verifies if the inverse has already been calculated. If so, it just returns it. Otherwise, it calculates the inverse of the data and sets it in the cache using the setmean function.

cacheSolve <- function(x, ...) {
        m <- getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}

