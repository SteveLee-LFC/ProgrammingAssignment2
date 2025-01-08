## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL  ## Initialize the cache for the inverse as NULL.
        
        ## Function to set the value of the matrix.
        set <- function(y) {
                x <<- y  ## Set the matrix value.
                m <<- NULL  ## Reset the cached inverse to NULL when the matrix changes.
        }
        
        ## Function to get the current value of the matrix.
        get <- function() x 
        
        ## Function to set the cached inverse of the matrix.
        setInverse <- function(inverse) m <<- inverse
        
        ## Function to get the cached inverse of the matrix.
        getInverse <- function() m
        
        ## Return a list of all functions
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        m <- x$getInverse()  ## Check if the inverse is already cached.
        
        if(!is.null(m)) {
                message("getting cached data")  ## If cached, return the cached inverse.
                return(m)
        }
        
        data <- x$get()  ## Get the matrix from the object.
        m <- solve(data, ...)  ## Compute the inverse of the matrix.
        x$setInverse(m)  ## Cache the computed inverse.
        m  ## Return the computed inverse.
}



## Create a 2x2 matrix
#my_matrix <- matrix(c(4, 7, 2, 6), nrow = 2)

## Create a special matrix object and cache it
#cached_matrix <- makeCacheMatrix(my_matrix)

## Compute and cache the inverse of the matrix
#inverse_matrix_1 <- cacheSolve(cached_matrix)
#print("First calculation (inverse matrix):")
#print(inverse_matrix_1)

## Calling cacheSolve again should fetch the inverse matrix from the cache 
## instead of recalculating it
#inverse_matrix_2 <- cacheSolve(cached_matrix)
#print("Second calculation (cached inverse matrix):")
#print(inverse_matrix_2)
