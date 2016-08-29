## Put comments here that give an overall description of what your
## functions do


## Usage example:
## > m
##      [,1] [,2] [,3] [,4]
## [1,]    1    0    0    3
## [2,]    2    1    0    0
## [3,]    0    4    1    0
## [4,]    0    0    8    3
## > cachem <- makeCacheMatrix(m)
## > cacheSolve(cachem)
##             [,1]        [,2]        [,3]         [,4]
## [1,] -0.01587302  0.50793651 -0.12698413  0.015873016
## [2,]  0.03174603 -0.01587302  0.25396825 -0.031746032
## [3,] -0.12698413  0.06349206 -0.01587302  0.126984127
## [4,]  0.33862434 -0.16931217  0.04232804 -0.005291005
## > cacheSolve(cachem)
## getting cached data
##             [,1]        [,2]        [,3]         [,4]
## [1,] -0.01587302  0.50793651 -0.12698413  0.015873016
## [2,]  0.03174603 -0.01587302  0.25396825 -0.031746032
## [3,] -0.12698413  0.06349206 -0.01587302  0.126984127
## [4,]  0.33862434 -0.16931217  0.04232804 -0.005291005


## Function makeCacheMatrix creates a matrix object 
## which can cache inverse matrix of itself.
## Usage: for a given matrix m, try
## >  cachem <- makeCacheMatrix(m)
## then cacheable matrix object will be assigned to variable cachem.

makeCacheMatrix <- function(x = matrix()) {

	# initialize
	inv <- NULL
	
	# set a matrix
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	
	# get the given matrix
	get <- function() x
	
	# set inverse of the matrix
	setinv <- function(inverse) inv <<- inverse
	
	# get inverse of the matrix
	getinv <- function() inv
	
	# list what it has
	list (set = set, get = get, setinv = setinv, getinv = getinv )
}


## Function cacheSolve returns the inverse matrix of the given matrix 
## like the function solve,
## but it uses previously calculated and cached value if exists.
## Usage: for a given makeCacheMatrix object cachem, try
## > cacheSolve(cachem)

cacheSolve <- function(x, ...) {
        ## try to get inverse from cache
		inv <- x$getinv()
		if (!is.null(inv)) {
			message("getting cached data")
			return (inv)
		}
		
		# if cache does not have a value
		mat <- x$get()
		inv <- solve(mat)
		
		# save calculated value to cache
		x$setinv(inv)
		
		# return
		inv
}
