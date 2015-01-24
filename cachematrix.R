## these two functions compute the inverse of a square matrix. If the inverse of the 
## matrix has already been computed, it will be retrieved from cache.

## This function creates a special  "matrix" object that can cache its inverse.
## it actually returns a list of functions to
## set a matrix, get a matrix, set the inverse and get the inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
	
	set <- function(y) {
	    x <<- y
		inverse <<- NULL
	}
	
	get <- function() x
	
	setinverse <- function(iv) inverse <<- iv
	
	getinverse <- function() inverse
	
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)	
}

## this function returns the inverse of the special "matrix" created by the first function.
## If the inverse has already been calculated, it will be retrieved from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inverse <- x$getinverse()
		if (!is.null(inverse)) {
		    message("getting cached data")
		    return(inverse)
		}
		mx <- x$get()
		inverse <- solve(mx, ...)
		x$setinverse(inverse)
		inverse
}
