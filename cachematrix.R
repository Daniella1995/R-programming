# This function creates a special "matrix" object that can cache its inverse (Assuming the matrix is always invertible):
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y  
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x) {
    m <- x$getInverse()
    if(!is.null(m)) { 
        return(m)
    }

    data <- x$get()
    m <- solve(data)
    x$setInverse(m)
    m 
}
