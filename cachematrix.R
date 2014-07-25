## Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {      ## defines the funtion makeCacheMatrix
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }                                            ## Method to set the matrix
    get <- function() { x
    }
    
    setInverse <- function(inverse) {  m <<- inverse   ## Sets the inverse of the matrix
    }
    getInverse <- function() {  m                ## Gets the inverse of the matrix
    }
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)                ## Returns a list of the methods
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) { ## defines the funtion cacheSolve
        m <- x$getInverse()
        if( !is.null(m) ) {
            message("getting cached data")
            return(m)          ## Simply returns the inverse if its already set
        }
        data <- x$get()
        m <- solve(data)       ## returns the inverse x if x is a square invertible matrix
        x$setInverse(m)        ## Set the inverse to the object
        m
    }

