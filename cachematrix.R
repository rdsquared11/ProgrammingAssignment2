## These two functions create a special matrix object and then compute
## its inverse and then cache it so it can be retrieved.

## This function creates a special matrix object that can cache its
## own inverse.

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
          x <<- y
          im <<- NULL
        }
        get <- function() x
        setinv <- function(solve) im <<- solve
        getinv <- function() im
        list(set = set, 
             get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the matrix from makeCacheMatrix
## function and can retrieve the inverse from the cache if already solved.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
        im <- x$getinv()
        if (!is.null(im)) {
          message("getting cached data")
          return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setinv(im)
        im
}
