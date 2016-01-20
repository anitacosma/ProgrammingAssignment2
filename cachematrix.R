## Caching the inverse of a matrix, to avoid additional computation costs when calling the function with same data

## This function creates a special "matrix" object that can cache its inverse(list of functions to several additional operations)

makeCacheMatrix <- function(x = matrix()) {
	CachedInv <- NULL
	set <- function(y) {
	x <<- y
	CachedInv <<- NULL
	}
	get <- function() x
	setinverse <- function(inv) CachedInv <<- inv
	getinverse <- function() CachedInv
	list(set = set, get = get,
		 setinverse = setinverse ,
		 getinverse = getinverse )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	MatrixInv <- x$getinverse()
	if(!is.null(MatrixInv)) {
		message("getting cached matrix")
		return(MatrixInv)
	}
	matrixData <- x$get()
	MatrixInv <- solve(matrixData,...)
	x$setinverse(MatrixInv)
	MatrixInv
}
