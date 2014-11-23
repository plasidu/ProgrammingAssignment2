## Followin two functions are supposed to be used as a way to compute matrix inverse efficiently, eg. use previously cached value of matrix inverse and return it in case, it hadn't changed.

## Example:
# > amatrix = makeCacheMatrix(matrix(1:4, 2, 2))
# > cacheSolve(amatrix)
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(amatrix)
# getting cached data
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5


## Function makeCacheMatrix takes matrix as an argument and makes a list of functions, which are meant to be used in function cacheSolve in order to determine, if the matrix inverse had been calculated previously.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x 
        setinvers <- function(invers) m <<- invers
        getinvers <-  function() m
        list(set = set, get = get,
                   setinvers = setinvers,
                   getinvers = getinvers)
}


## Function cacheSolve takes matrix as an argument, checks whether input matrix invers has been calculated via function in makeCacheMatrix, else the function computes the inverse. In both cases, matrix inverse is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinvers()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinvers(m)
        m

}
