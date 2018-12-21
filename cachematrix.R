## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {# Input a matrix
        i <- NULL                          # Initialize i as NULL
        set <- function(y) {                # Set the value of the matrix
                x <<- y                     # Assign to x the value of input argument
                i <<- NULL                  # Reset the inverse matrix i as NULL since x is newly assigned
        }
        get <- function() x                 # Retrieve x
        setinverse <- function(inverse) i <<- inverse   # Assign to i the value of input argument
        getinverse <- function() i          # Retrieve the cached value of the inverse matrix i
        list(set = set,                     # Create a list of the above functions that can be called using $
             get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {            # Input a makeCacheMatrix object as an argument
        i <- x$getinverse()                 # Assign to i the cached value, if there is not a cached value, the value of i will be NULL
        if(!is.null(i)) {                   # If the value of i is not NA, i.e. there is a cached value, this simply returns the cached value without doing any calculation
                message("getting cached data")
                return(i)
        }
        data <- x$get()                     # Assign to data the cached matrix
        i <- solve(data, ...)               # Get the inverse of the cached matrix and assign it to i
        x$setinverse(i)                     # Cache the newly calculated inverse to the makeCacheMatrix object
        i                                   # Return a matrix that is the inverse of 'x'
}
