## This file contains two functions. The first (makeCacheMatrix) is a prototype function that will
## create a set of functions for getting and setting a matrix and its inverse value.

## The second function (cacheSolve) makes use of the makeCacheMatrix functions to either return
## the cached inverse value or recalculate the inverse value and store it back in the makeCacheMatrix object.

## Example usage;
## a <- matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)
## b <- makeCacheMatrix(a)
## c <- cacheSolve(b)


## Define a Prototype function that will create getter and setter functions
## that will be used by the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {

        i <- NULL  ## Initialise the symbol that will contain the inverse value
        
        ## Create a function that will store the matrix that will be inversed
        set <- function(y) {
                x <<- y         ## Set values in the parent environment
                i <<- NULL 
        }
        
        ## Create a function that will retrieve the matrix over which the inverse was applied
        get <- function() x

        ## Create a function that will store the inverse value        
        setInverse <- function(inverse) i <<- inverse  ## Set value in the parent environment
        
        ## Create a function that will retrieve the inverse value
        getInverse <- function() i
 
        ## Return a vector of the functions to enable the $ form of the extract operator to access the functions by name    
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)        
        
}


## Calculate the Inverse of a matrix or return the cached value if it exists.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'.
        ## 'x' is an instance of the makeCacheMatrix object that will contain a matrix to inverse.

        ## Attempt to retrieve the already calculated inverse value from within the 'x' object by 
        ## referring to the appropriate 'x' function
        i <- x$getInverse()
        
        ## If the inverse value already exists, just return it and end. No need to recalculate it.
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        ## No cache exists so retrieve the matrix to inverse. The matrix value was stored in the 'x' object
        ## when the object was instantiated.
        data <- x$get()
        
        ## Calculate the inverse of the retrieved matrix
        i <- solve(data)
        
        ## Store the inverse value in the 'x' object using the appropriate 'x' function
        x$setInverse(i)
        
        i   ## Return the inverse value             
        
}
