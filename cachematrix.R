## makeCacheMatrix function is used to create a special object that stores 
## a numeric matrix and cache's its inverse

## it returns a list of functions:
## set:         set the value of the matrix
## get:         get the value of the matrix
## setinverse:  set the value of the inverse
## getinverse:  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function calculates the inverse of the special list 
## created by makeCacheMatrix function.

## it firstly checks to see if the inverse has already been calculated.
## if so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of
## the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
