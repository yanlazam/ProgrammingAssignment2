## The following functions calculate the inverse of a matrix and saves it
## to the cache such that the next time the user attempts to calculate the
## matrix inverse, the previously saved value is returned instead of
## repeating the calculation.

## This function creates a special "matrix" object, which is really a list 
## containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        ## create a matrix object x and some associated sub-functions/methods
        
        ## define the cache c
        c <- NULL
        set <- function(y) {
                x <<- y ## assign the input matrix y to the variable x in the parent environment
                c <<- NULL ## re-initialize c in the parent environment to null
        }
        get <- function() x ## return the matrix x
        setinverse <- function(inverse) c <<- inverse ## set the cache c equal to the inverse of the matrix x
        getinverse <- function() c ## return the cached inverse of x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" created
## with the above function. However, it first checks to see if the inverse
## has already been caclulated. If so, it 'get's the inverse from the cache
## and skips the computation. Otherwise, it calculates the matrix inverse
## and sets the value of the inverse in the cache via the 'setinverse' function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        c <- x$getinverse()
        if(!is.null(c)) {
                message("getting cached data")
                return(c)
        }
        data <- x$get()
        c <- solve(data, ...)
        x$setinverse(c)
        c
}

## Usage example:
##
## > source('cachematrix.R')
## > m <- makeCacheMatrix(matrix(c(2, 0, 0, 2), c(2, 2)))
## > cacheSolve(m)
##      [,1] [,2]
## [1,]  0.5  0.0
## [2,]  0.0  0.5
## > cacheSolve(m)
##  getting cached data
##     [,1] [,2]
##[1,]  0.5  0.0
##[2,]  0.0  0.5
