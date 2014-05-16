##  This program is almost exactly like the mean Vector example provided,
##  but operates on a matrix instead and calculates its inverse using 
##  R's built-in solve() function.  It was tested in R studio and will
##  calculate the inverse of a square matrix.

##  To use this script, call makeCacheMatrix passing it a square matrix. 
##  For example m1 is the matrix:                       
##       [,1] [,2]
##  r1    1    2
##  r2    3    1
##  > mspec <- makeCacheMatrix(m1)
##  > solved <- cacheSolve(mspec)
##  > solved
##         r1   r2
##  [1,] -0.2  0.4
##  [2,]  0.6 -0.2

## This function takes a square matrix, calculates its inverse and caches the solution.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) m <<- solve
    getSolve <- function() m
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}
## This function takes an argument of type List created by makeCacheMatrix and will returne 
## the cached solution, or calculate a solution if not in the cache.
cacheSolve <- function(x, ...) {
    m <- x$getSolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setSolve(m)
    m
}