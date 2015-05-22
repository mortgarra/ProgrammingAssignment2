## These functions implement a simple cache for computationally
## expensive matrix calculations.

## makeCacheMatrix constructs a list of functions that can be
## used to access and set both the cached matrix and matrix inverse
## This function is consumed by cacheSolve below.
makeCacheMatrix <- function(cacheMatrix = matrix()) 
{
	cacheMatrixInverse <- NULL

	#Mutator to set the internal matrix
	setMatrix <- function(newCacheMatrix)
	{
		cacheMatrix <<- newCacheMatrix
		cacheMatrixInverse <<- NULL
	}
	#Accessor to retrieve the internal matrix
	getMatrix <- function() 
	
	{
		cacheMatrix
	}

	#Returned the cached inverse of X
	getMatrixInverse <- function()
	{
		cacheMatrixInverse
	}

	#Set the cached inverse of X
	setMatrixInverse <- function(newCacheMatrixInverse)
	{
		cacheMatrixInverse <<- newCacheMatrixInverse
	}

	list(set = setMatrix, get = getMatrix, 
		 getInverse = getMatrixInverse, 
		 setInverse = setMatrixInverse)
}


##Retrieve the matrix inverse of the matrix stored within cacheMatrix.
##Either retrieve the already computed value, or compute and set the
##cached value accordingly.
cacheSolve <- function(cacheMatrix, ...) 
{
    ## Check to see if the inverse is already calculated
    inverse <- cacheMatrix$getInverse()
    if(!is.null(inverse))
    {
    	#Found it!
    	return(inverse)
    }

    #No cached copy available.  Solve now and pass any
    #parameters to the solver routine.  

    #If we were being clever, we'd also detect to see if 
    #the parameters passed to us were different than what 
    #were originally used to generated our cached matrix 
    #inverse.  But we're not clever, now are we?
    inverse <- solve(cacheMatrix$get(), ...)
    
    #Cache the copy
    cacheMatrix$setInverse(inverse)

    #Return the newly computed inverse
    return(inverse)
}
