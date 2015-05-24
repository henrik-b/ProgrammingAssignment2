## cachematrix.R
##
## Henrik Behrens, 2015
##
## In this file, we provide a function makeCacheMatrix to create a special matrix
## and a function cacheSolve to calculate it's inverse.
## The result of the inverse calculation is cached.
## When the cacheSolve function is called a second time, the cached result is returned
## without recalculating the inverse. 

# The first function, `makeMatrix` creates a special "matrix", which is
# really a list containing functions to
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse
# 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
	get <- function() x
	setinv <- function(inv) i <<- inv
	getinv <- function() i
	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv)
}

# The following function calculates the inverse of the special "matrix"
# created with the above function. However, it first checks to see if the
# inverse has already been calculated. If so, it `get`s the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the data and sets the value of theinverse in the cache via the `setinv`
# function.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	i <- x$getinv()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinv(i)
	i
}
