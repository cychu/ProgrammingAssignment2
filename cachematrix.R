## Write a pair of functions that cache the inverse of a matrix

## Create a matrix called makeCacheMatrix which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function()x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function()i
    list(set=set, get=get
         setinverse=setinverse,
         getinverse=getinverse)
}


## CacheSolve is a function which computes the inverse of the matrix 
## returned by "mackCacheMatrix". If the inverse has been calculated
## , cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <-x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data,...)
    x$setinverse(i)
    i
    }