## Overall, these two functions store the contents of a matrix, calculate the inverse of the matrix
## and store the inverse. The aim is to save computation time of inverting a matrix by storing the inverse.

## The makeCacheMatrix function stores a matrix and also stores its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinv <- function(sol= matrix()) {
        inverse <<- sol
    }
    getinv <- function() {
        return(inverse)
    }
    list(set = set, get = get,
    setinv = setinv,
    getinv = getinv)
}


## The cacheSolve function determines if the inverse of a given matrix has been solve. If so, it recovers the ##inverse; if no inverse is found it determines the inverse and stores it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inverse <- x$getinv()
        if(!is.null(inverse)) {
            message("getting cached matrix")
            print(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinv(inverse)
        inverse
}
