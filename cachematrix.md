## Put comments here that give an overall description of what your
## functions do
	##Functions for Cache of inverse of matrix. 
	###firstly, setting the matrix, then,getting the matrix and its inverse.
	###secondly, computing the inverse of matrix using cache solve asuming an invertible matrix.

## Write a short comment describing this function
	##Function for Cache of inverse of matrix. 
	###firstly, setting the matrix, then,getting the matrix.
	###secondly, setting the inverse of matrix, then,getting the inverse of matrix.then listing


makeCacheMatrix <- function(x = matrix()) {
inv <- NULL							## Initialize the inverse
    set <- function(y) {					## set the matrix
        x <<- y
        inv <<- NULL
    }
    get <- function() x						## get the matrix
    setinverse <- function(inverse) {
	inv <<- inverse		
	}							## set the inverse of the matrix
    getinverse <- function() {
	inv
	}							## get the inverse of the matrix
    list(set=set, get=get, 
	 setinverse=setinverse, getinverse=getinverse)		## get the list

}


## Write a short comment describing this function
	## The following function Computes the inverse of the special matrix created by "makeCacheMatrix"
	## above. . However, it first checks to see If the inverse has already been calculated.
	##If so then the "cachesolve" should retrieve the inverse from the cache and skip computation.
	## otherwise, it calculated the inverse of the matrix and sets the value of the inverse in the cache via the function.
	
	# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()					## Return the inverse of 'x'
        if(!is.null(inv)) {						## Return the inverse if its already set
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)						#### Calculate the inverse of matrix
    x$setinverse(inv)
    inv

}
