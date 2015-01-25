## The below functions helps you get the inverse of matrices
## Assumptions made while doing so are: The input matrix is a square matrix
## Also, the inverse for the matrix provided exists.
## The cacheSolve Function with the help of the makeCacheMatrix function persists the state
## of the matrix and its inverse and returns a fresh value the first time, and then returns the values
## calculated earlier

## Function Name: makeCacheMatrix
## Arguments: x :: Sample matrix on which the operations will be done.
## Returns : A list of functions namely: set, get, setInverse, getInverse
## set function: Sets the value of the matrix on which the operations will be done.
## get function: Gets the value of the matrix that had been set earlier
## setInverse: argument: inv :: sets the inverse of the matrix.
## getInverse: acts as a cache and return the inverse of the matrix calculated earlier.

makeCacheMatrix <- function(x = matrix()) {
	
	inverse <- NULL

        set <- function(y){
                mat <<- y
                inverse <<- NULL
        }

        get <- function() mat
        
	setInverse <- function(inv) inverse <<- inv
        
	getInverse <- function() inverse
        
	list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Function Name: cacheSolve
## Fixed Arguments: x: Special matrx vector returned by makeCacheMatrix function
## This function checks if the inverse of the matrix has already been calculated 
## If already calculated returns the same, else calculates the inverse, sets the inverse
## into the special vector for future use and returns the inverse as well. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	##Get the value of inverse set in the special vector returned by makeCacheMatrix
	inverse <- x$getInverse()
	
	## If the value of the inverse is not NULL, then calculate the inverse
	##  and set the value back to the special vector for future use.

        if(!is.null(inverse)) {
                message("Getting Inverse from Cache")
                return(inverse)
        }
        givenMatrix <-x$get()
        inv<-solve(givenMatrix)
        x$setInverse(inv)
        inv	
}
