## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the cached inverse as NULL

    # Function to set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL  # Reset the inverse when the matrix is updated
    }

    # Function to get the matrix
    get <- function() x

    # Function to set the cached inverse
    setinverse <- function(inverse) inv <<- inverse

    # Function to get the cached inverse
    getinverse <- function() inv

    # Return a list of the above functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# Function to compute or retrieve the cached inverse of the matrix
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()  # Check if the inverse is already cached
    if (!is.null(inv)) {
        message("getting cached inverse")
        return(inv)  # Return the cached inverse
    }

    # If not cached, compute the inverse
    data <- x$get()
    inv <- solve(data, ...)  # Compute the inverse
    x$setinverse(inv)  # Cache the computed inverse
    inv
}

