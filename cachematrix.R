## Coursea R Programming
## Programming Assignment 2: Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse
## The matrix is assumed to be square and invertible
## makeCacheMatrix creates a special "vector", which is really a list containing a function to
##   1. set the value of the matrix
##   2. get the value of the matrix
##   3. set the value of the matrix inverse
##   4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed)
## then cacheSolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinverse(inv)
        ## Return a matrix that is the inverse of 'x'
        inv
}


## set.seed(0)
n <- 5
mat <- matrix(runif(n * n), n, n)
mat
f <- makeCacheMatrix()
f$set(mat)
f$get()
## f$setinverse(solve(f$get()))
cacheSolve(f)
cacheSolve(f)
mat %*% f$getinverse()
