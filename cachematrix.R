## makeCacheMatrix creates a special "matrix" object that can cache its inverse 

# Assumptions   :the matrix supplied is always invertible 

# Returns       :a list of functions to get/set the matrix and it's inverse

# Functions: 
# * set         :stores the value of the matrix 
# * get         :retrieves the value of the cached matrix 
# * setinverse  :stores (caches) the inverse of the matrix 
# * getinverse  :retrieves the cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    # initialize inverse of matrix object
    n <- NULL
    
    # set value of matrix
    set <- function(y) {
        # check if supplied matrix is identical to stored matrix
        # only assign new matrix and reset inverse matrix if new matrix is different
        if(!identical(x,y))
        {
            x <<- y
            n <<- NULL
        }
    }
    
    # retrieve matrix
    get <- function() x
    
    # set the value of the inverse of matrix object
    setinverse <- function(invMatrix) n <<- invMatrix
    
    # retrieve the inverse of the matrix object
    getinverse <- function() n
    
    # return a list of get/set functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns the inverse of the makeCacheMatrix matrix

# Returns       :the inverse of the matrix

cacheSolve <- function(x, ...) {
    # retrieve the 'cached' inverse of the matrix
    n <- x$getinverse()
    
    # if the inverse of matrix object is not null, display 'getting cached data' message
    # else, generate inverse and store (cache) value
    if(!is.null(n)) {
        message("getting cached data")
    } else {
        data <- x$get()
        n <- solve(data, ...)
        x$setinverse(n)
    }
    
    # return inverse of matrix
    n
}
