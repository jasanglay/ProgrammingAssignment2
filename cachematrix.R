## Put comments here that give an overall description of what your
## functions do

## The function, makeCacheMatrix creates a special "matrix", which 
## is really a list containing a function to set the value, get 
## the value, set the value of the inverse, and get the value of 
## the inverse of the matrix. This function creates a special 
## "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function, cacheSolve calculates the inverse of the special 
## "matrix" created with the function, makeCacheMatrix. It first 
## checks to see if the inverse has already been calculated. If 
## so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the mean of the data and
## sets the value of the inverse in the cache via the setinverse
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
