## Below functions will cache the inverse of a matrix passed to the function.

## makeCacheMatrix : takes input of an matrix and creates a list of functions to cache inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
	# inv is inverse. Initially set to NULL
	inv <- NULL 

	# set matrix data
    set <- function(y) {
            x <<- y
            inv <<- NULL
    }

    # returns matrix data
    get <- function() x

    # set the inverse in cache
    setinv <- function(inverse){ 
    		inv <<- inverse 
    }

    # return the cached inverse
    getinv <- function() { 
    	inv 
    }

    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv) 
}


## cacheSolve : this comutes inverse of a matrix made by makeCacheMatrix
## If inverse is already made this function will return the inverse retrieved from the cache.
cacheSolve <- function(x, ...) {
  # inv is inverse
  	# get inverse from cache
	inv <- x$getinv()
	# if inv is already cached, return the cache.
	if(!is.null(inv)) {
	        message("getting cached data")
	        return(inv)
	}
	## if inv is not cached, return the inverse of the matrix and set in the cache.
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}
