## The following pair of functions cache the inverse of a matrix
## It is assumed that the matrix passed to the function is always invertible

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y) {
                x<<-y
                m<<-NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<-solve
        getinverse <- function() m
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null(m)) {
                message("Getting Cached Data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m        
}
