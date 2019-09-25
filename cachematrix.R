## These functions work together to compute and cache the inverse of a matrix

## This function functions as a special object to store and retrieve a matrix 
## and its inverse - it is the "store" function

## !! The input matrix needs to an invertible square matrix !! 

makeCacheMatrix <- function(x = matrix()) {
    #Pass the function an input matrix
    matrix <- NULL
    set <- function(y) {
        x <<- y
        matrix <<- NULL
    }
    ## get argument used to retrieve input matrix
    get <- function() x
    ## set_solve used to cache inverse 
    set_solve <- function(solve) matrix <<- solve
    ## get_solve used to retrieve inverse
    get_solve <- function() matrix
    list(set = set, get = get, set_solve = set_solve, get_solve = get_solve)

}


## This function is the "compute" function - pass it the result of the 
## makeCacheMatrix function and it returns the inverse of the input matrix.
## It doesn't recompute it if it has already been computed and cached once.

cacheSolve <- function(x, ...) {
    matrix <- x$get_solve()
    if (!is.null(matrix)) {
        message("Getting cached data.")
        return(matrix)
    }
    data <- x$get()
    matrix <- solve(data,...)
    x$set_solve(matrix)
    matrix
}

## Testing functionality

test <- matrix(runif(25,0,100),5,5)
test_list <- makeCacheMatrix(test)
test_inverse <- cacheSolve(test_list)
test_cache <- cacheSolve(test_list)
if (identical(test_inverse,test_cache)) message("Cache identical.")


