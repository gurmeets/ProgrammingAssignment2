## This file has functions to create a matrix whose inverse is cacheable
## and to retreive or calculate the inverse as required. 

## This function creates a matrix whose inverse can be cached

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL 
	}
	get <- function() {
		x
	}
	setinverse <- function(data) {
		i <<- data
	}
	getinverse <- function() {
		i
	}
	## The special matrix is really a list containing functions to 
	## set and get the values of matrix and the inverse 
	list(set = set, 
		 get = get, 
		 getinverse = getinverse, 
		 setinverse = setinverse)
}


## This function checks if inverse of the matrix is cached
## It returns the cached inverse if any, else calculates the inverse and caches it

cacheSolve <- function(x, ...) {
		## Check if the inverse is already cached
        i <- x$getinverse()
        if(!is.null(i)) {
        		message("getting cached data")
        		return(i)
        }
        ## Since it is not cached, solve the matrix and cache the inverse
        data <- x$get()
        i <- solve(data, ...) 
        x$setinverse(i)
        i
}
