## TThe function below creates a special "matrix", from a list containing a 
## function that it does:

## 1) Sets the values of the entries in the matrix.
## 2) Gets the matrix.
## 3) Sets the values of each entry in the inverse matrix.
## 4) Gets the inverse matrix.

## This function creates a special "matrix" object that can store its inverse.

makeCacheMatrix <- function(x = matrix()) {
    mInv <- NULL
    set <- function(y) {
        x <<- y
        mInv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) mInv <<- inverse
    getInv <- function() mInv
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## This function calculates the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve should retrieve the inverse from the 
## cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mInv <- x$getInv()
    if (!is.null(mInv)) {
        message("Getting chached data")
        return(mInv)
    }
    mtrx <- x$get()
    mInv <- solve(mtrx, ...)
    x$setInv(mInv)
    mInv
}
