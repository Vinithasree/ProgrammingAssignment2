## Vinithasree - Solution to Programming Assignment 2
## Matrix inversion is usually a costly computation and it will be useful 
## to cache the inverse of a matrix than to compute it repeatedly.
## The below two functions can be used to create a special object that stores 
## a matrix and to cache its inverse

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## The set function assigns the input matrix to x
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## The get function can be called to get the input matrix x
    get <- function() x
    ## The setinverse function receives the inverse as its input and caches it
    setinverse <- function(inverse) m <<- inverse
    ## The getinverse function can be called to get the cached inverse value 
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by 
## the above defined makeCacheMatrix function.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve function will retrieve the inverse from the cache.
## Note: x must be a square invertible matrix

cacheSolve <- function(x, ...) {
    ## The getinverse function is called to get the cached inverse value, if any 
    m <- x$getinverse()
    
    ## If inverse has already been calculated and cached, 'm' will not be null, and 
    ## the below if condition will return the retrieved inverse value 'm'
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## If no cached inverse value is found, the below lines calculate the 
    ## inverse of the matrix 'x' and also store/cache the inverse using the 
    ## setinverse() function. 
    data <- x$get()
    m <- solve(data, ...) ## Calculate inverse
    x$setinverse(m)
    m
}
