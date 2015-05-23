# Description
# makeCackeMatrix creates a function wrapper for a matrix and its inverse. 
# The wrapper can be initialized with a matrix, or is by default initialized
# with an empty matrix.
#
# The wrapper exposes 4 functions for setting and getting the matrix, and the
# inverse of the matrix. These functions are:
#     setMatrix(matrix = matrix)
#     getMatrix()
#     setInverse(inverse = inverse)
#     getInverse()
#
# Usage
#     Initialization
#         > cacheMatrix <- makeCacheMatrix() # Empty matrix
#         OR
#         > matrix <- matrix(1:4, ncol = 2, nrow = 2)
#         > cacheMatrix <- makeCacheMatrix(matrix) # 2 x 2 matrix
#
#     Setting and getting matrix
#         > matrix <- matrix(1:4, ncol = 2, nrow = 2)
#         > cacheMatrix <- makeCacheMatrix()
#         > cacheMatrix$setMatrix(matrix) 
#         > cacheMatrix$getMatrix() 
#     NOTE: Setting the matrix resets the inverse to NULL
#
#     Setting and getting inverse
#         > matrix <- matrix(1:4, ncol = 2, nrow = 2)
#         > inverse <- solve(matrix)
#         > cacheMatrix <- makeCacheMatrix(matrix)
#         > cacheMatrix$setInverse(inverse) 
#     NOTE: Setting the inverse does not validate that the inverse is
#     correct for the matrix
makeCacheMatrix <- function(x = matrix()) {
    message("Initializing cache matrix")
    matrix <- x
    inverse <- NULL
    
    setMatrix <- function(matrix) {
        message("Setting matrix")
        matrix <<- matrix
        
        message("Resetting inverse to NULL")
        inverse <<- NULL
    }
    
    getMatrix <- function() {
        matrix
    }
    
    setInverse <- function(inverse) {
        message("Setting inverse")
        inverse <<- inverse
    }
    
    getInverse <- function() {
        inverse
    }
    
    message("Returning function list")
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse, getInverse = getInverse)
}

# Description
# cacheSolve function takes in an initialized makeCacheMatrix and sets the
# makeCacheMatrix with the inverse of the matrix in makeCacheMatrix.
# 
# Notes
# If the makeCacheMatrix is not valid and does not conform to the 
# expectations of an initialized makeCacheMatrix, cacheSolve does nothing
# and returns with a warning.
# If the matrix in makeCacheMatrix is not set or is not a matrix cacheSolve 
# does nothing and returns with a warning.
#
# Usage:
#     > matrix <- matrix(1:4, nrow = 2, ncol = 2)
#     > cacheMatrix <- makeCacheMatrix(matrix)
#     > cacheSolve(cacheMatrix)
#     > cacheMatrix$getInverse() # returns the inverse
cacheSolve <- function(x, ...) {
    inverse <- NULL
    
    message("Validating makeCacheMatrix")
    
    if (class(x) != "list") {
        warning("x is not a makeCacheMatrix, it does not return a list")
    } else if (class(x[["getMatrix"]]) != "function") {
        warning("x is not a makeCacheMatrix, it does not have a getMatrix function")
    } else if (class(x[["getInverse"]]) != "function") {
        warning("x is not a makeCacheMatrix, it does not have a getInverse function")
    } else if (class(x[["setInverse"]]) != "function") {
        warning("x is not a makeCacheMatrix, it does not have a setInverse function")
    } else {
        message("The makeCacheMatrix is valid")
        
        inverse <- x$getInverse()
        
        if(is.null(inverse)) {
            message ("Solving matrix inverse")
            matrix <- x$getMatrix()
            
            try(inverse <- solve(matrix, ...), silent = T)
            
            if (is.null(inverse)) {
                warning("x does not contain an invertible matrix")
            } else {
                x$setInverse(inverse)
            }
        }
    }
    
    inverse
}
