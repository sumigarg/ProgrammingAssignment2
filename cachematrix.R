## Put comments here that give an overall description of what your
## functions do

## Function to define get,set,getinverse,setinverse functionality to enable caching 
## of the inverse of a matrix
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


## Function to implement the cache using makeCacheMatrix function
## 1. Sets the inverse of a matrix in a cache if not found in cache
## 2. Else return the inverse from the cache and prints the message
##    "getting cached data"

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


