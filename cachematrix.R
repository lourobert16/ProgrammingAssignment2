## Put comments here that give an overall description of what your
## functions do

## Creates a special matrix (really a list) that can store its inverse

makeCacheMatrix <- function(x = matrix()) {
	ci <- NULL
	set <- function(y) {
		x <<- y
		ci <<- NULL
	}
	get <- function() x
	setInv <- function(inverse) ci <<- inverse
	getInv <- function() ci
	list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Computes the inverse of the special matrix created by makeCacheMatrix, unless the inverse is stored in the cache

cacheSolve <- function(x, ...) {
	ci <- x$getInv()
	if(!is.null(ci)) {
		message("getting cached data")
		return(ci)
	}
	mat <- x$get()
	ci <- solve(mat)
	x$setInv(ci)
	ci
}
