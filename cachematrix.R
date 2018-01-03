## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##function makeCacheMatrix is passed a matrix as input
## set is setting the value of the matrix
## get is getting the value of the matrix
## setinverse is setting the inverse Matrix
## getinverse is getting the inverse Matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) m <<- inv
    getinverse <- function() m
    list(set = set
        ,get = get
        ,setinverse = setinverse
        ,getinverse = getinverse)
}

##function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above
##If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {  
    m <- x$getinverse()  
    if(!is.null(m)) {  
        message("getting cached data")
        return(m)  
    }  
    data <- x$get()  
    m <- solve(data, ...)  
    x$setinverse(m)  
    ## Return a matrix that is the inverse of 'x'
    m  
}  

##test of the above functions
a  <- matrix(2:4,2,2)
ca <- makeCacheMatrix(a)
ca$getinverse()
## output a
##     [,1] [,2]
##[1,]    2    4
##[2,]    3    5

## output invers matrix ca
##     [,1] [,2]
##[1,] -2.5    2
##[2,]  1.5   -1