## Goal of these functions is to cache the inverse of a matrix

## This function initializes the matrix and contains a set of functions 
## used to cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

	# 1. initialize inverse variable
	inv <- NULL

	# 2. set function - initialize/set matrix
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	
	# 3. get function - return matrix
	get <- function() x
	
	# 4. setinverse function - set inverse to cache
	setinverse <- function(inverse) inv <<- inverse
	
	# 5. getinverse function - retrieve inverse from cache
	getinverse <- function() inv
	
	# 6. list of functions
	list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This function is used to compute the inverse of the input matrix, such that
## it should return the inverse from the cache if the inverse has already been calculated

cacheSolve <- function(x, ...) {
	
	# 1. get inverse from cache
	inv <- x$getinverse()
	
	# 2. check if inverse already calculated
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	
	# 3. get matrix to invert
	data <- x$get()

	# 4. solve for inverse of matrix
	inv <- solve(data, ...)

	# 5. set inverse to cache
	x$setinverse(inv)

	# 6. return inverse
	inv

}
