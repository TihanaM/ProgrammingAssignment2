## It can be potentially time consuming to calculate the inverse of a matrix. 
## If the matrix is not changing, it might be benefitial to cache the value
## of the inverse of the matrix, so when it is needed in the future, it can
## be only looked up, and not calculated from scratch.The makeCacheMatrix and 
## cacheSolve functions are used to cache the inverse of a matrix.

## makeCacheMatrix creates a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}



## cacheSolve will return a matrix that is the inverse of 'x'.
##The function first checks if the inverse of the matrix has been already 
## calculated. It the value already exists, it skips the calculation, types 
## "getting cached data." and returns the value. Otherwise, it calculates
## the inverse of the matrix in the cache via the setinverse function.

## It is assumed that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data.")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}


