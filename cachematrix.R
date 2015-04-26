## 
## filename: cachematrix.R
## 
## this file contains 2 functions makeCacheMatrix and cacheSolve and those functions
##
## makeCacheMatrix creates a special vector object that can cache the inverse of a matrix.
##  i.e. store inverse value for later re-use
## This is useful for re-use of this inverse instead of doing inverse everytime is needed and not waste compute resoruces/time
##  This function is really a list containing functions
##      1) set the value of the vector
##      2) get the value of the vector
##      3) set the value of the inverse
##      4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    # initiate cache inverse to null
    cacheinv <- NULL    
    
    # set the value of the matrix 
    set <- function(y) {
        x <<- y
        cacheinv <<- NULL
    }
    # get/return the value of the matrix
    get <- function() x
    
    # set the value of the inverse (i.e. store in the cache)
    setinv <- function(inv) cacheinv <<- inv
    
    # return/get the value of the inverse (that was cached earlier or a null)
    getinv <- function() cacheinv
    
    # a list of the functions made by this function
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## cacheSolve: is a function that calculates the inverse of the special "vector" created
## with the above function makeCacheMatrix. 
## However, it first checks to see if the inverse has already been computed to avoid re-computation. 
##   If it is, it gets the inverse from the cache and skips the computation. 
##   If not, it computes the inverse of the data/matrix and sets the value of the inverse 
##     in the cache using the setinv function, for later re-use

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    
    if(!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    # inverse is not found in the cache.  Therefore get the data and calculate/store in cache
    data <- x$get()
    inv <- solve(data, ...)      # solve that solves a system of equations can give the inverse of a matrix
    x$setinv(inv)
    inv
}
