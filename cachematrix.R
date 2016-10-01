## Programming Assignment 2: Lexical Scoping
## Requires to write a R function that is able to cache potentially time-consuming 
## computation - Matrix Inversion. This is usually a costly computation and there
## may be benefit to cache the inverse of a matrix rather than compute it repeatedly.

## Assignment is to write 2 functions
## Function 1: makeCacheMatrix 
## This function creates a special "matrix" object that can cache its inverse.
## Function 2: cacheSolve
## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then the cacheSolve should retrieve the inverse from the cache.

## makeCacheMatrix: 
## This function creates a special "matrix" object that can cache its inverse
##
## Details of creating a makeCacheMatrix object will consist of 
## Four functions encapsulated in a list 
## 	1. set the matrix 
## 	2. get the matrix 
## 	3. set the inverse of the matrix 
## 	4. get the inverse of the matrix 

## Arguments: 
## x    A square invertible matrix; 
##      For this assignment - matrix is assumed to be always invertible
##
## Usage: 
## makeCacheMatrix(x)
##
## Example:
## sq_matrix <- matrix(rnorm(9), nrow=3, ncol=3) 
## cachedM <- makeCacheMatrix(sq_matrix) 
##
## if you give this function a non-invertible matrix then you should see a error message
##
## nonInvertibleMatrix <- (1:9, nrow=3, ncol=3) 
## cachedM <- makeCacheMatrix(sq_matrix) 
## This should error and following message should be printed on console 
## "Error: Matrix is not invertible, cannot convert to special matrix"

makeCacheMatrix <- function(x = matrix()) {
	## Confirm whether the matrix is invertible
	if (det(x) == 0) {
		print("Error: Matrix is not invertible, cannot convert to special matrix")
		return;
	}

	inverseM <- NULL

	## Set the matrix, does not calculate the inverse
	setMatrix <- function(y) {
		# Assign variables to parent environment
		x <<- y
		inverseM <<- NULL
	}

	## Get the matrix only
	getMatrix <- function() x

	## Set the inverse matrix
	setInverse <- function(invM) inverseM <<- invM

	## Get the inverse matrix
	getInverse <- function() inverseM 

	## Collate the get and set functions in the list
	list(set = setMatrix, get = getMatrix,
			setInverseM = setInverse, getInverseM = getInverse)
}


## cacheSolve: 
## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then the cacheSolve should retrieve the inverse from the cache.
##
## Arguments: 
## x    special matrix - returned by makeCacheMatrix; 
##
## Usage: 
## cacheSolve(x)
##
## Example:
## >sq_matrix <- matrix(rnorm(9), nrow=3, ncol=3) 
## >cachedM <- makeCacheMatrix(sq_matrix) 
## >solveM <- cacheSolve(cachedM) 
## >solveM
##         [,1]  [,2]  [,3]
## [1,] -7.049 3.413 4.234
## [2,] -3.927 1.597 2.916
## [3,] -5.509 2.048 3.008
## 
## >solveM_2<- cacheSolve(m) 
## [1] "Cached data retrieved" 
## >solveM_2
## you should see same output as the one previously printed for print(solveM)

cacheSolve <- function(x, ...) {
	## Retrieve the inverse matrix
	inverseMatrix <- x$getInverseM ()

	## Check if value exists - this would imply that cached value is returned
	## Print a message for user and return the value
      if(!is.null(inverseMatrix )) {
		print("Cached data retrieved")
		## Return a matrix that is the inverse of 'x'
		return(inverseMatrix )
	}

	## if value does not exist, retrieve the matrix
	data <- x$get()

	## inverse the matrix. 
	## No need to check "data" for square invertible matrix as that has been taken care
	## within makeCacheMatrix function
	inverseMatrix <- solve(data, ...)

	## cache the inverse matrix for later retrieval
	x$setInverseM (inverseMatrix )

	## Return a matrix that is the inverse of 'x'
	inverseMatrix 
}
