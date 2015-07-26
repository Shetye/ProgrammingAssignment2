## The two functions given here together compute inverse of a matrix and keep track of 
## whether or not the inverse already exits.  If it exists, then 
## it is obtained from cache.
##
## This function produces a list of the following 4 function
## 1) setmatrix: function to define matrix x
## 2) getmatrix: function to get matrix x
## 3) setinv: set the value of the inverse of matrix x
## 4) getinv: get the value of the inverse of matrix x
##
makeCacheMatrix <- function(x = matrix()) {
        xinv <- NULL
#        
        setmatrix <- function(y){
                x <<- y
                xinv <<- NULL
        }
#
        getmatrix <- function() x
#
        setinv <- function(solve)  xinv <<- solve
#
        getinv <- function() xinv
#
        list(setmatrix=setmatrix, getmatrix=getmatrix,
             setinv=setinv, getinv=getinv)
}
## Function CacheSolve calculates the inverse of the matrix x defined in 
## the function setmatrix().  However, it first checks to see if the 
## inverse has been calculated.  If so, it gets the inverse from the cache
## and skips the computation.  Otherwise, it calculates the inverse of the
## matrix set by setmatrix above and sets the value of xinv in the cache
## via the setinv function.
##
cacheSolve <- function(x, ...) {
        xinv <- x$getinv()
        if (!is.null(xinv)) {
                message("getting cached data")
                return(xinv)
        }
        data <- x$getmatrix()
        xinv <- solve(data, ...)
        x$setinv(xinv)
        xinv
        ## Return a matrix that is the inverse of 'x'
}
