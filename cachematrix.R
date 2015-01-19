## Put comments here that give an overall description of what your
## functions do

## Function that creates matrix with cached inverse
## x - is input matrix

makeCacheMatrix <- function(x = matrix()) {
    invm <- NULL ##init inverse of matrix to NULL
    ## function to set the data of matrix
    set <- function(y) {
        x <<- y
        invm <<- NULL
    }
    ## function to get matrix data
    get <- function() x
    ## function to compute inverse of matrix and save it ti invm
    setinverse <- function(solve) invm <<- solve
    ## function to get inverse of matrix
    getinverse <- function() invm
    ## list of CacheMatrix functions
    list(set=set, get=get, setinverse=setinverse,
         getinverse=getinverse)
}


## Function that returns inverse of matrix 'x'
## Input is CacheMatrix

cacheSolve <- function(x, ...) {
    ## get inverse of matrix
    invm <- x$getinverse()
    ## check if inverse of matrix is not NULL
    if(!is.null(invm)){
        message("Inverse of matrix already computed, get data from cache")
        return(invm)
    }
    ## if inverse is not cached get matrix, compute inverse and save it to cache
    data <- x$get() 
    message("Computing inversed matrix and save it to cache")
    invm <- solve(data) ## compute inverse
    x$setinverse(invm) ## save to cache
    invm ## return the value
}
