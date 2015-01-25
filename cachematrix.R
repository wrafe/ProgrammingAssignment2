## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix creates a special "matrix" object that can cache it's inverse.
## What it actually does is create a list containing functions to :
## set - sets the value of the matrix
## get - gets the value of the matrix
## setInverse - sets the value of the inverse of the matrix
## getInverse - gets the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    # variable used to hold the inverse of the matrix
    inver <- NULL
	
    # set function
    set <- function(y) {
        x <<- y
        inver <<- NULL
    }
	
    # get function
    get <- function() x
	
    # setInverse function
    setInverse <- function(inverse) inver <<- inverse
	
    # getInverse function
    getInverse <- function() inver
	
    # list containing getter and setter functions (or methods)
    list(set = set, 
         get = get, 
         setinverse = setInverse, 
         getinverse = getInverse)
}


## Write a short comment describing this function

## cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
    # get the inverted matrix
    inver <- x$getinverse()
	
    # check if the inverse has been computed.  If so, return it
    if(!is.null(inver)) {
        message("getting cached data")
        return(inver)
    }
	
    # Since inverse has not been computed, get the matrix, 
    # compute the inverse using the solve function and set the
    # value in the cache using the setinverse function
    mtrix <- x$get()
    inver <- solve(mtrix)
    x$setinverse(inver)
    inver
}
