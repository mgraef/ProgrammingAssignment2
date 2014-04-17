## The following pair of functions cache the inverse of a matrix
## It is assumed that the matrix passed to the function is always invertible

## To run:
## Store an invertible matrix in a variable.  For Example: tmat<-matrix(c(3,6,2,7,4,8,1,9,0),nrow=3,ncol=3)
## Pass this variable to makeCacheMatrix and store in a second variable.  xx<-makeCacheMatrix(tmat)
## Pass the second variable to cacheSolve.  For Example: cacheSolve(xx)


## This function (makeCacheMatrix) creates a special "matrix" object that can cache its inverse.
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


## This function (cacheSolve) computes the inverse of the special "matrix" returned by makeCacheMatrix
## above.  If the inverse has already been calculated (and the matrix has not changed), then the 
## cacheSolve function will retrieve the inverse from the cache.
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
