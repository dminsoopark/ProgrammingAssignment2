## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix will initiate variables x & inverse
## inverse will be used later, in cacheSolve. makeCahceMatrix
## is also responsible for setting up the 'getters' so that
## we will be able to call functions as arguments, as well
## as make use of the scoping rules in R
## cacheSolve is going to utilize the get function from
## makeCacheMatrix to see if an existing inverted matrix
## exists and call it, or create it using the solve() func

## Write a short comment describing this function
## makeCacheMatrix initializes x as a type, matrix. also
## initializes the variable, inverse, as well as the functions
## for set, get, setmatrix, and getmatrix
makeCacheMatrix <- function(x = matrix()) { 
        inverse <- NULL 
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrix) inverse <<- matrix
        getmatrix <- function() inverse
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## Write a short comment describing this function
## cacheSolve is the function that actually creates or gets
## the inversed matrix from the environment. it will first
## check if an inversed matrix exists, and return it if it
## does. if not, it will run sovle and set it as an inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getmatrix()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setmatrix(inverse)
        return(inverse)
}
