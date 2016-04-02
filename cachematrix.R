## These functions cache a matrix and its inverse, to avoid recalculating its inverse if it's already known.

## makeCacheMatrix() creates a special matrix object that can cache the matrix itself, 
## and can cache its inverse. If "M" is an invertible matrix, and "M-1" its inverse,
## makeCacheMatrix(M) will cache M, and makeCacheMatrix()$setinverse("M-1") will cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
            m<<-NULL
            set <- function(y) {           ## "set" function is to cache the Matrix ("m" gets NULL).
              x <<- y
              m <<- NULL
            }
            get <- function() x            ## "get" returns the Matrix currently cached.

            setinverse <- function(inverse) m <<- inverse        ## "setinverse" function is to cache its inverse (manually)
            getinverse <- function() m                           ## "getinverse" returns its inverse

            list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve() checks if the inverse of the matrix hasn't been cached yet. 
## If it has been cached, then "getinverse()" is not NULL and cacheSolve
## returns the inverted matrix without any operation. 
## If not, it gets the matrix cached previously in makeCacheMatrix() ( x$get() ) 
## to calculate (and cache) its inverse.

cacheSolve <- function(x, ...) {
          m <- x$getinverse()      ## if the inverse has been cached then m is not NULL
          if(!is.null(m)) {
          message("getting cached data")
          return(m)
            }
          data <- x$get()          ## Otherwise, the function gets the matrix cached previously
          m <- solve(data, ...)
          x$setinverse(m)          ## and cache the inverse.
          m
}
