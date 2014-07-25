## makeCacheMatrix function
##
## Description: returns a object with all the functions for operation the matrix
##
## Returns: object containing the matrix, initialized inverse, and associated functions
##
## Detailed: 
## 1 - initialize the inverse matrix (m) variable with NULL
## 2 - create the set matrix function
## 3 - create the get matrix function
## 4 - create the setinverse function - sets the inverse matrix calculation
## 5 - create the getinverse function - return the inverse matrix
## returns the list of functions available
##
## Functions created:
##      get, set: matrix
##      getinverse, setinverse: inverse matrix
##

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
        
}


## cacheSolve function
##
## Description: Verifies if the inverse matrix is cached, if not, calculete and cache it
##
## Returns: inverse matrix
## 
## Detailed:
##
## 1 - Retrieves the content of the cached inverse matrix
## 2 - if it is not NULL use the cached version
## 3 - otherwise gets the matrix and calculates its inverse using solve()
## 4 - cache the calculated inverse matrix
## 5 - return the inverse matrix
##

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached inverse matrix")
                return (m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
}
