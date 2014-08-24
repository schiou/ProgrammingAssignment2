
## The makeCacheMatrix function is resposible for creating a new matrix 
## object which will then be called by the "cacheSolve" function to perform 
## tasks required for this assignment. This function contain codes that will 
## determine whether a 'new' calculation is required or a result is already 
## stored previously. Additionally, this function contains functions (get/set) 
## that are utilized in the cacheSolve function. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) m <<- Inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function serves to first check to see if the inverse of the 
## function has already been previously calculated. If the inverse 
## of the input matrix has been calculated previously, the function will 
## return the inverse matrix that is stored. If the inverse has never 
## been calculated before, this function will calculate the inverse 
## value, store the result and return the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
