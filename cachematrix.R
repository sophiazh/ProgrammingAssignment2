## Project: Coursera > R Programming > Assignment2


## makeCacheMatrix and cacheSolve matrix use lexical scoping to cache values of complex computation
## so that the after initial calling, the results are stored for next use without the need to 
## re-calculate

## makeCacheMatrix: create an makeCacheMatrix object 
## that has set/get, setInverse/getInverse functions.
## the inverse of the matrix is stored in the enclosing environment by using <<- to modify
## variable value in the parent environment, which is makeCacheMatrix
## Input: an invertable matrix
## Output: a list of set/get functions


makeCacheMatrix <- function(x = matrix()) {
          
          invrs <- NULL             # initialize invrs to NULL
          
          set <- function(y) {      # reset input matrix to a new matrix
                    x <<- y         # assign new matrix to x in the enclosing environment
                    invrs <<- NULL  # reset invrs in the enclosing environment
          }
          
          
          get <- function() {x}     # return the input matrix


          setInverse <- function(inverse) {
                    invrs <<- inverse # assign new inverse value to invrs in enclosing environment
          }


          getInverse <- function {invrs} # return invrs

          list(set = set, get = get,     # return a list of the four functions
               setInverse = setInverse, 
               getInverse = setInverse)
}


## cacheSolve: return the inverse of a matrix object created by makeCacheMatrix
## first time called: calculate the inverse and store result in the matrix
## after first time: retrieve cached inverse from the matrix object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          
          invrs <- x$getInverse() # retrieve inverse from the matrix object
          
          # if the inverse is availabe, return the value
          if (!is.null(invrs)) {  
                    message("getting cached data")
                    return(invrs)
          }
          
          # if the inverse is null, calculate the inverse
          data <- x$get()  # retrieve matrix from the matrix object
          
          invrs <- solve(data, ...) # solve inverse
          
          x$setInverse(invrs) # assign solved inverse to the matrix object
          
          invrs # return solved inverse
}
