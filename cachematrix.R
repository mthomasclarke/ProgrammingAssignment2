## Prepared for R Programming MOOC for Program Assignment 2 
## July 25 2015
## MBTC
##
## Cache the inversion of a matrix to eliminate needless
## recalculations of preivious determined values
##
##  Assumes there is an inverse of the matrix
## 
##  Two functions - One to manage the values of the matrix
##  One to determine if whether a new inverse is needed
##  If not get the value from the stored cache
##  if so, calculate the inverse
## 
## 
## 
## makeCacheMatrix manages the cache. The four functions will either 
## return the matrix, changes the matrix, stores the value of the inverse
## and returns the value of the inverse. 

## The cashesolve fucntion wil use the values to pair up the input
## and the inverse to see if it exists or not and set the new input and 
## inverse if need be.
##
##

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        setmatrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(solveit) m <<- solveit
        getinverse <- function() m
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## Cache solves determines if the inverse has already been calculated.
## If it has it pulls the inverse of the matrix from the cache. 
## If not, it calculates the inverse.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$getmatrix()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}








