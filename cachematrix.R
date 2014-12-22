## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix must be initialized with a type matrix object in order to use cacheSolve. it is assumed that the passed matrix is invertable. the matrix saved to makeCacheMatrix can be inverted by passing the object to cacheSolve

## Write a short comment describing this function

## makeCacheMatrix creates an object with the capabilities to (1) set the value of the matrix, (2) get the value of the matrix, (3) set the value of the matrix inverse, and (4) get the value fo the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
	
	inverse_matrix <- NULL
	set <- function(y) {
		x <<- y
		inverse_matrix <<- NULL
	}
	get <- function() x
	set_inverse <- function(inverse) inverse_matrix <<- inverse
	get_inverse <- function() inverse_matrix
	list (set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
	

}


## Write a short comment describing this function

## cacheSolve takes a matrix object that has been initialized by the makeCacheMatrix function, returns the cached inverse if previously calculated, and otherwise calculates and returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inverse_matrix <- x$get_inverse()
        if (!is.null(inverse_matrix)) {
        	message("getting cached data")
        	return(inverse_matrix)
        }
        
        data <- x$get()
        inverse_matrix <- solve(data)
        x$set_inverse(inverse_matrix)
        inverse_matrix
        
}

## test case:

## matrixTest <- matrix(c(4, 3, 3, 2), nrow = 2, ncol = 2)
## cacheMatrixTest <- makeCacheMatrix(matrixTest)
## solve(matrixTest)
## cacheSolve(cacheMatrixTest) ## note result matches solve(matrixTest)
## cacheSolve(cacheMatrixTest) ## note that "getting cached data" is displayed, indicating the cache was accessed
