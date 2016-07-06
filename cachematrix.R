## this pair of function can create a special object that stores 
## the a matrix and caches its Inverse matrix.

## The first function, makeCacheMatrix creates a list containing 
## a function to

## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse
## 4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        get <- function() x
        setinverse <- function(Inverse) Inv <<- Inverse
        getinverse <- function() Inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## the second function can check if there is a cache of the inverse 
## already at first. If so, it gets the inverse from the cache and 
## skips the computation. Otherwise, the function can calculate the 
## inverse of the given matrix and save the value of inverse in 
## the cache.

cacheSolve <- function(x, ...) {
        Inv <- x$getinverse()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        data <- x$get()
        Inv <- solve(data, ...)
        x$setinverse(Inv)
        Inv ## Return a matrix that is the inverse of 'x'
}
