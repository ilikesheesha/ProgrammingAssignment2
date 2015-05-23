library(testthat)
source("cachematrix.R")

context("makeCacheMatrix tests")

test_that(
    "makeCacheMatrix initializes by default correctly", {
    cacheMatrix <- makeCacheMatrix()
    
    expect_equal(cacheMatrix$getMatrix(), matrix())
    expect_equal(cacheMatrix$getInverse(), NULL)
    expect_equal(length(cacheMatrix), 4)
    expect_is(makeCacheMatrix()[["setMatrix"]], "function")
    expect_is(makeCacheMatrix()[["getMatrix"]], "function")
    expect_is(makeCacheMatrix()[["setInverse"]], "function")
    expect_is(makeCacheMatrix()[["getInverse"]], "function")
})

test_that(
    "makeCacheMatrix sets new matrix correctly", {
    cacheMatrix <- makeCacheMatrix()
    cacheMatrix$setInverse("just for testing")
    expect_equal(cacheMatrix$getInverse(), "just for testing")
    
    matrix <- matrix(1:4, ncol = 2, nrow = 2)
    cacheMatrix$setMatrix(matrix)
    
    expect_equal(cacheMatrix$getMatrix(), matrix)
    expect_equal(cacheMatrix$getInverse(), NULL)
    expect_equal(length(cacheMatrix), 4)
    expect_is(makeCacheMatrix()[["setMatrix"]], "function")
    expect_is(makeCacheMatrix()[["getMatrix"]], "function")
    expect_is(makeCacheMatrix()[["setInverse"]], "function")
    expect_is(makeCacheMatrix()[["getInverse"]], "function")
})

test_that(
    "makeCacheMatrix sets inverse correctly", {
    matrix <- matrix(1:4, ncol = 2, nrow = 2)
    inverse <- solve(matrix)
    cacheMatrix <- makeCacheMatrix(matrix)
    cacheMatrix$setInverse(inverse) 
    
    expect_equal(cacheMatrix$getMatrix(), matrix)
    expect_equal(cacheMatrix$getInverse(), inverse)
    expect_equal(length(cacheMatrix), 4)
    expect_is(makeCacheMatrix()[["setMatrix"]], "function")
    expect_is(makeCacheMatrix()[["getMatrix"]], "function")
    expect_is(makeCacheMatrix()[["setInverse"]], "function")
    expect_is(makeCacheMatrix()[["getInverse"]], "function")
})

test_that(
    "makeCacheMatrix comments examples are correct", {
    #Initialisation - default
    cacheMatrix <- makeCacheMatrix() # Empty matrix
    
    expect_equal(cacheMatrix$getMatrix(), matrix())
    expect_equal(cacheMatrix$getInverse(), NULL)
    expect_equal(length(cacheMatrix), 4)
    expect_is(makeCacheMatrix()[["setMatrix"]], "function")
    expect_is(makeCacheMatrix()[["getMatrix"]], "function")
    expect_is(makeCacheMatrix()[["setInverse"]], "function")
    expect_is(makeCacheMatrix()[["getInverse"]], "function")
    
    #Initialisation - with matrix
    matrix <- matrix(1:4, ncol = 2, nrow = 2)
    cacheMatrix <- makeCacheMatrix(matrix) # 2 x 2 matrix
    
    expect_equal(cacheMatrix$getMatrix(), matrix)
    expect_equal(cacheMatrix$getInverse(), NULL)
    expect_equal(length(cacheMatrix), 4)
    expect_is(makeCacheMatrix()[["setMatrix"]], "function")
    expect_is(makeCacheMatrix()[["getMatrix"]], "function")
    expect_is(makeCacheMatrix()[["setInverse"]], "function")
    expect_is(makeCacheMatrix()[["getInverse"]], "function")
    
    #Setting and getting matrix
    matrix <- matrix(1:4, ncol = 2, nrow = 2)
    cacheMatrix <- makeCacheMatrix()
    cacheMatrix$setMatrix(matrix) 
    cacheMatrix$getMatrix() 
    
    expect_equal(cacheMatrix$getMatrix(), matrix)
    expect_equal(cacheMatrix$getInverse(), NULL)
    expect_equal(length(cacheMatrix), 4)
    expect_is(makeCacheMatrix()[["setMatrix"]], "function")
    expect_is(makeCacheMatrix()[["getMatrix"]], "function")
    expect_is(makeCacheMatrix()[["setInverse"]], "function")
    expect_is(makeCacheMatrix()[["getInverse"]], "function")
    
    #Setting and getting inverse
    matrix <- matrix(1:4, ncol = 2, nrow = 2)
    inverse <- solve(matrix)
    cacheMatrix <- makeCacheMatrix(matrix)
    cacheMatrix$setInverse(inverse) 
    
    expect_equal(cacheMatrix$getMatrix(), matrix)
    expect_equal(cacheMatrix$getInverse(), inverse)
    expect_equal(length(cacheMatrix), 4)
    expect_is(makeCacheMatrix()[["setMatrix"]], "function")
    expect_is(makeCacheMatrix()[["getMatrix"]], "function")
    expect_is(makeCacheMatrix()[["setInverse"]], "function")
    expect_is(makeCacheMatrix()[["getInverse"]], "function")
})

test_that(
    "cacheSolve correctly handles a NULL makeCacheMatrix", {
    expect_warning(
        cacheSolve(NULL), 
        "x is not a makeCacheMatrix, it does not return a list")
})

test_that(
    "cacheSolve correctly handles a null makeCacheMatrix", {
    expect_warning(
        cacheSolve(NA), 
        "x is not a makeCacheMatrix, it does not return a list")
})

test_that(
    "cacheSolve correctly handles an invalid makeCacheMatrix - not list", {
    expect_warning(
        cacheSolve("invalid makeCacheMatrix"), 
        "x is not a makeCacheMatrix, it does not return a list")
})

test_that(
    "cacheSolve correctly handles an invalid makeCacheMatrix - missing getMatrix", {
    expect_warning(
        cacheSolve(list(x = "invalid makeCacheMatrix")), 
        "x is not a makeCacheMatrix, it does not have a getMatrix function")
})

test_that(
    "cacheSolve correctly handles an invalid makeCacheMatrix - missing getInverse", {
    expect_warning(
        cacheSolve(list(getMatrix = function() {})), 
        "x is not a makeCacheMatrix, it does not have a getInverse function")
})

test_that(
    "cacheSolve correctly handles an invalid makeCacheMatrix - missing setInverse", {
    expect_warning(
        cacheSolve(
            list(getMatrix = function() {}, getInverse = function(){})), 
        "x is not a makeCacheMatrix, it does not have a setInverse function")
})

test_that(
    "cacheSolve correctly handles valid empty makeCacheMatrix", {
    inverse <- solve(matrix())
    expect_equal(cacheSolve(makeCacheMatrix()), inverse)
})

test_that(
    "cacheSolve correctly handles an invertable makeCacheMatrix - case 1", {
    matrix <- matrix(c(-1, -2, 1, 1), nrow = 2, ncol = 2)
    inverse <- solve(matrix)
    
    cacheMatrix <- makeCacheMatrix(matrix)
    expect_equal(cacheSolve(cacheMatrix), inverse)
    
    expect_equal(cacheMatrix$getMatrix(), matrix)
    expect_equal(cacheMatrix$getInverse(), inverse)
    expect_equal(length(cacheMatrix), 4)
    expect_is(makeCacheMatrix()[["setMatrix"]], "function")
    expect_is(makeCacheMatrix()[["getMatrix"]], "function")
    expect_is(makeCacheMatrix()[["setInverse"]], "function")
    expect_is(makeCacheMatrix()[["getInverse"]], "function")
})

test_that(
    "cacheSolve correctly handles an invertible makeCacheMatrix - case 2", {
    matrix <- matrix(1:4, nrow = 2, ncol = 2)
    inverse <- solve(matrix)
    
    cacheMatrix <- makeCacheMatrix(matrix)
    expect_equal(cacheSolve(cacheMatrix), inverse)
    
    expect_equal(cacheMatrix$getMatrix(), matrix)
    expect_equal(cacheMatrix$getInverse(), inverse)
    expect_equal(length(cacheMatrix), 4)
    expect_is(makeCacheMatrix()[["setMatrix"]], "function")
    expect_is(makeCacheMatrix()[["getMatrix"]], "function")
    expect_is(makeCacheMatrix()[["setInverse"]], "function")
    expect_is(makeCacheMatrix()[["getInverse"]], "function")
})

test_that(
    "cacheSolve correctly handles an invertible makeCacheMatrix - case 3", {
    matrix <- matrix(c(3, 2, 4, 7), nrow = 2, ncol = 2)
    inverse <- solve(matrix)
    
    cacheMatrix <- makeCacheMatrix(matrix)
    expect_equal(cacheSolve(cacheMatrix), inverse)
    
    expect_equal(cacheMatrix$getMatrix(), matrix)
    expect_equal(cacheMatrix$getInverse(), inverse)
    expect_equal(length(cacheMatrix), 4)
    expect_is(makeCacheMatrix()[["setMatrix"]], "function")
    expect_is(makeCacheMatrix()[["getMatrix"]], "function")
    expect_is(makeCacheMatrix()[["setInverse"]], "function")
    expect_is(makeCacheMatrix()[["getInverse"]], "function")
})

test_that(
    "cacheSolve correctly handles a non-invertable makeCacheMatrix - case 1", {
    matrix <- matrix(1:9, nrow = 3, ncol = 3)
    
    cacheMatrix <- makeCacheMatrix(matrix)
    expect_warning(cacheSolve(cacheMatrix), 
                   "x does not contain an invertible matrix")
    
    expect_equal(cacheMatrix$getMatrix(), matrix)
    expect_equal(cacheMatrix$getInverse(), NULL)
    expect_equal(length(cacheMatrix), 4)
    expect_is(makeCacheMatrix()[["setMatrix"]], "function")
    expect_is(makeCacheMatrix()[["getMatrix"]], "function")
    expect_is(makeCacheMatrix()[["setInverse"]], "function")
    expect_is(makeCacheMatrix()[["getInverse"]], "function")
})

test_that(
    "cacheSolve correctly handles a non-invertible makeCacheMatrix - case 2", {
    matrix <- matrix(1:25, nrow = 5, ncol = 5)

    cacheMatrix <- makeCacheMatrix(matrix)
    expect_warning(cacheSolve(cacheMatrix), 
                   "x does not contain an invertible matrix")
    
    expect_equal(cacheMatrix$getMatrix(), matrix)
    expect_equal(cacheMatrix$getInverse(), NULL)
    expect_equal(length(cacheMatrix), 4)
    expect_is(makeCacheMatrix()[["setMatrix"]], "function")
    expect_is(makeCacheMatrix()[["getMatrix"]], "function")
    expect_is(makeCacheMatrix()[["setInverse"]], "function")
    expect_is(makeCacheMatrix()[["getInverse"]], "function")
})