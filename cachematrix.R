## These two functions are created to cache the inverse of a special "matrix"
## object

## Sets and gets the value of the matrix and set the value of the inverse

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


## This function calculates the value of the inverse of the matrix. It first
## checks to see if the inverse has already been calculated. If so, it uses the
## cached value of the inverse. If it hasn't then it calculates the inverse and
## sets it using $setinverse.

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
