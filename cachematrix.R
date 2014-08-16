## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function is modelled on the sample for caching the mean
## This adding attributes to the matrix so the solution can be cached for later
## setsolve solves the inverse and stores it globally
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
		## this stores the matrix in a global storage
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
		## this attribute gets that stored matrix
        get <- function() x
		## this caches the solution to the inverse
        setsolve <- function(solve) m <<- solve
		## this gets that cached solution
        getsolve <- function() m
		## list of attributes
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Write a short comment describing this function
## This is also directly modelled on the sample given
cacheSolve <- function(x, ...) {
        ## Try to get the cached solution
		        m <- x$getsolve()
		## if there is a cached version, then give that as an answer
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
		
		## If there is not a cached version
        data <- x$get()
		## Solve the matrix
        m <- solve(data, ...)
		## and cache the solution for possible later use
        x$setsolve(m)
		## and return the solution
        m
}
