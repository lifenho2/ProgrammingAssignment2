## There are two functions here working together to return the inverse matrix for a given input
## When the input matrix has been computed before, return the value directly from the cache.
## The intention for this two functions are to speed up the computation for the inverse matrix,
## in particularly for matrix with higher dimensions, eg. 8*8 or 9*9

## The first function is makeCacheMatrix.
## This function consist of 4 sub functions to get and retrive the inverse values

makeCacheMatrix <- function(x = matrix()) {

	  ## Initialize the inv variable as NULL
        inv <- NULL

        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

        get <- function() x

        setinverse <- function(inverse) inv <<- inverse

        getinverse <- function() inv
        
	  list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

## This function will receive an input as matrix and then check if the value is already available in cache.
## If the value is already avaliable in cache, retrieve the value directly.
## If the value is not available, compute the inverse matrix with the "solve" function

cacheSolve <- function(x = matrix()) {

	  ## get the previous input matrix. 
	  previous_input <- x$get

	  ## Check if previous matrix input is the same as current input matrix value
	  if (!(x = previous_input)){
        	    x$set(x)  ## this will reset the inv value to NULL
  	  }

	  ## get the inverse value from the cache	
	  inv <- x$getinverse()

	  ## check if the cache consist of any value, if yes, directly return the value
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv) 		## exit the function and return inv value
        }

	  ## This part will only execute if cache is null.
	  ## Either run for the first time or not match with previous input matrix
        inv <- solve(x)     ## solve is the function used to inverse the input matrix
        x$setinverse(inv)   ## set the result into the cache
        inv			    ## return inv value
}
