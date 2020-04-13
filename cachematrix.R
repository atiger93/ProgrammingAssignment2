## Put comments here that give an overall description of what your
## functions do

## MATRIX definition for caching innverse

makeCacheMatrix <- function(x = matrix()) {
    invmat <- NULL
    set <- function(y) {
        x <<- y
        invmat <<- NULL
    }
    get <- function() x
    setinv <- function(solve) invmat <<- solve
    getinv <- function() invmat
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## compute the inverese of the chached matrix defined above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invmat <- x$getinv()
    if(!is.null(invmat)) {
        message("getting cached data")
        return(invmat)
    }
    special_mat <- x$get()
    invmat<- solve(special_mat, ...)
    x$setinv(invmat)
    invmat
}
