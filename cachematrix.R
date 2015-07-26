## The following function takes a matrix as a parameter and creates a special 
## matrix and caches in an envrionement variable

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL

        set <- function(y) {
                x <<- y
                m <<- NULL
        }

        get <- function() {
			x
	  }

        setInverse <- function(Inverse) {
			m <<- Inverse
	  }

        getInverse <- function() {
			m
	  }

        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## The following function takes the special cached matrix as a parameter and returns
## an invertible matrix. It returns the invertible matrix from the cache if it was 
## already inverted and it was in the cache. Otherwise it creates an inverted matrix
## from the scratch

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'

        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached Inversed matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}