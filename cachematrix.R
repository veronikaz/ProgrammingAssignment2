## Put comments here that give an overall description of what your
## functions do

# The two functions below will allow to cach an inverse of a Matrix.

## Write a short comment describing this function

# The function "makeCacheMatrix" creates a special "vector", which is a list containing a function to
# (1) set the value of the Matrix; (2) get the value of the Matrix; (3) set the value of the inverse of the Matrix
# (4) get the value of the inverse of the Matrix

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


## Write a short comment describing this function

# The function "cacheSolve" calculates the inverse of the Matrix created with the function "makeCacheMatrix". 
# It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the Matrix and sets the value of the inverse in the cache via the "setinverse" function.

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