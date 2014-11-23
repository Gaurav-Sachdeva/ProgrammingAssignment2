## The following two fuctions have been created in order to find out the inverse 
## of a matrix in an optimal manner. Function "makeCacheMatrix" creates a special 
## matrix object, and then the function "cacheSolve" calculates the inverse of 
## the matrix. If the matrix inverse has already been calculated, it will instead 
## find it in the cache and return it, and not calculate it again.

## The function makeCacheMatrix creates a special "matrix". This special matrix is a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of its inverse
## get the value of its inverse

makeCacheMatrix <- function(x = matrix()) {

	  ##Initializing the inverse with null value
	  i <- NULL

        ## Function to set the matrix				
        set <- function(y) {
                x <<- y
                i <<- NULL
        }

        ## Fuction to get the matrix
        get <- function() x

        ## Function to set the inverse of the matrix
        setinverse <- function(inverse) i <<- inverse

        ## Function to get the inverse of the matrix
        getinverse <- function() i

        ## Returning a list of functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function "cacheSolve" computes the inverse of the special matrix 
## returned by "makeCacheMatrix" above. If the inverse has already been 
## calculated and the matrix has not changed, then the "cachesolve" retrieves 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	  ## Retrieving the inverse from cache
	  i <- x$getinverse()

	  ## Checking if the inverse is not null, i.e., if inverse has already been calculated
        if(!is.null(i)) {
                message("getting cached data")

		    ## Returning the inverse
                return(i)
        }

        ## Getting the matrix from our object
        data <- x$get()

	  ## Calculating the inverse using "solve" function
        i <- solve(data, ...)

        ## Setting the inverse to the object
        x$setinverse(i)

        ## Returning the inverse
        i
}

