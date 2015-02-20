## makeCacheMatrix: creates a special "matrix" object that can cache its inverse.
## cacheSolve:      computes the inverse of the special "matrix" returned by makeCacheMatrix.
##                  If the inverse has already been calculated (and the matrix has not changed),
##                  then cacheSolve retrieves the inverse from the cache.


## makeCacheMatrix creates a special "matrix", which is a list containing functions to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve calculates the inverse of the special "matrix" created with the makeCacheMatrix function, by 
## getting it from the cache or calculates the inverse of the matrix and sets the value of the it in the cache
## via the setinverse function.
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}