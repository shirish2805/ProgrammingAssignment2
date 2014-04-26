
## makeCacheMatix function uses the lexical scope in R to cache the matrix and its inverse
## it returns a list of following four functions 
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
## the input 'x' to this function is the matrix to be cached
makeCacheMatrix <- function(x = matrix()) {
	
	i <- NULL
	set <- function(y) {
		x <<- y;
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inv) i <<- inv
	getinverse <- function() i
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns the inverse of the matrix from the cache if it already exists
## otherwise it calculates the inverse, updates the cache and returns the inverse
## the input 'x' to this function is the result of the function makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	i <- x$getinverse()
	if (!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}

