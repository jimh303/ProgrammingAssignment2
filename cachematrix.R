## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a matrix from the parameter x and establishes getter/setters
## for the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    invNewMatrix <- NULL
    set <- function(y) {
        x <<- y
        invNewMatrix <<- NULL
    }
    get <- function() x
    setInverse <- function(invMatrix) invNewMatrix <<- invMatrix
    getInverse <- function() invNewMatrix
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
     
  
}


## cacheSolve checks to see in the cacheMatrix has an inverse cached, if so it
## returns the cached value, if not it calculates it, sets it in the cache, and 
## then returns it

cacheSolve <- function(x, ...) {
    inv_x <- x$getInverse()
    if (is.null(inv_x)) {
        print("Inverse not cached - calculating...")
        inv_x <- solve(x$get())
        x$setInverse(inv_x)
    }
    else {print("Using cached value")}
    inv_x 
}
