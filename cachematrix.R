## Coursera R programming week 2 Assignment

## USAGE: 
## minv <- makeCacheMatrix()
## minv$set(matrix(c(10,2,2,10), 2, 2)) or other invertible matrix
## minv$get()
##cacheSolve(minv)

## This function, makeCacheMatrix, creates a special "matrix" which is a list of
## nested functions used to move data in and out of the cache environment in order## to perform operations on these data . The function does the following:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inversion
## 4. get the value of the matrix inversion

makeCacheMatrix <- function(x = matrix()) {
	minv <- NULL
	set <- function(y) {
		x <<- y
		minv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) minv <<- inverse
	getinverse <- function() minv
	list(set = set, get = get,
	    setinverse = setinverse,
	    getinverse = getinverse)

}


## This function cacheSolve returns the inverse of the special "matrix" which was ##created with by the makeCaheMatrix function above.
## It first checks if the inverse has already been calculated, if calculated, it 
## obtains the result and skips the recalculation else, it calculates the inverse
## and sets the value in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
         minv <- x$getinverse()

	 ## returns already calculated inverse if available
	 if(!is.null(minv)){
		message("getting cached data.")
		return(minv)
	 }

	 ## calculates inverse not yet computed and Cache the inverse meaning minv
         ## was NULL
	 data <- x$get()
	 minv <- solve(data, ...) 
	 x$setinverse(minv)
	 
        ## Returns a matrix that is the inverse of 'x' if cache inverse is not NULL
	 minv
}




## TEST RESULTS:

## > minv <- makeCacheMatrix()
## > minv$set(matrix(c(10,2,2,10), 2, 2))
## > minv$get()
##      [,1] [,2]
## [1,]   10    2
## [2,]    2   10
## > cacheSolve(minv)
##             [,1]        [,2]
## [1,]  0.10416667 -0.02083333
## [2,] -0.02083333  0.10416667
## > minv$set(matrix(c(0,2,2,0), 2, 2))
## > minv$get()
##      [,1] [,2]
## [1,]    0    2
## [2,]    2    0
## > cacheSolve(minv)
##      [,1] [,2]
## [1,]  0.0  0.5
## [2,]  0.5  0.0
## > cacheSolve(minv)
## getting cached data.
##      [,1] [,2]
## [1,]  0.0  0.5
## [2,]  0.5  0.0

