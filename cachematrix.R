##
## Caching the Inverse of a Matrix is usually a costly computation
## and there is benefit to caching the inverse of a matrix
## rather than computing it repeatedly.
## These functions cache the inverse of a matrix.
##

## The makeCacheMatrix creates a special "vector", which is
## really a list containing functions to
## - set the value of the vector
## - get the value of the vector
## - set the value of the inverse
## - get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    # for this case inverse and solve are synonyms
    # The R function for the inverse is called solve
    # for our application the user terminology
    # being used is inverse
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

##
## The cacheSolve function calculates the inverse of the special "vector"
## created with the above function.
## It first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of
##   the inverse in the cache via the setmean function.
##
cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'

    # check the cache
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }

    # Not found in the cache
    data <- x$get()
    # Do the intensive caluclation here
    m <- solve(data)
    # store the result
    x$setinverse(m)
    # return the answer
    m
}
