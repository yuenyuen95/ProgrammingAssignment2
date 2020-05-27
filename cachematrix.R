## I am creating a pair of functions makeCacheMatrix and cacheSolve that work hand in hand that can cache
## the inverse of matrices and save computing time

## The makeCacheMatrix function created a special object that can cache the inverse of the matrix, x

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setInverse <- function(solve) inv <<- solve
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The cacheSolve function calculates the inverse of the special object returned by the makeCacheMatrix function.
## If the inverse has already been calculated, the function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        
        ## Return a matrix that is the inverse of 'x'
        
        return(inv)
}

## To test this pair of functions, run the following lines in the console: 
        ## m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)

## Manually calculate the inverse of the test matrix, m1
        ## solve(m1)

## Use the myCacheMatrix and cacheSolve
        ## myMatrix_object <- makeCacheMatrix(m1)
        ## cacheSolve(myMatrix_object)

## You should be able to return the same inversed matrix