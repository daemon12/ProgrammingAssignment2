## FUNCTIONS FOR CREATING A MATRIX with CACHED VALUE FOR IT'S INVERSE

## This function creates a special "matrix" object, which has a list of functions for:
##    setting the value of the matrix
##    getting the value of the matrix
##    setting the value of the inverse of matrix
##    getting the value of the matrix of matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() {
		x
	}
	setinv <- function(i) {
		inv <<- i
	}
	getinv <- function() {
		inv
	}
	list(set = set, get = get, setinv = setinv,getinv = getinv)
}


## This function calculates the inverse of the special "matrix" created with above function.
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the input matrix and sets the 
## value of the inverse in the cache via the setinv function

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("Getting cached data for input matrix.")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv		
}
