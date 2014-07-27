## This code creates two functions which work in tandem to create
## a special matrix variable which stores an invertible matrix
## and its inverse. An invertible matrix is inputted into the
## function makeCacheMatrix and this output is inputted into
## cachSolve which either returns the inverse of the matrix
## already stored in the special matrix variable or creates and
## stores it back into the special matrix variable before it
## return the inverse matrix. The user interface is as follows:
##
## > m <- matrix(1:4,2,2)           // create invertible matrix m
## > m                              // show its contents
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
##
## > m1 <- makeCacheMatrix(m)       // make matrix variable m1
## > m1$get()                       // display the matrix m
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
##
## > m1$getinverse()                // display the inverse of m
## NULL                             // none yet, need to create it
##
## > cacheSolve(m1)                 // create and output inverse of m
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## > m1$getinverse()                // now m1 has the inverse of m
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## > cacheSolve(m1)                 // if we use same matrix again
## getting cached inverse           // it uses the cached version
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## > m %*% m1$getinverse()          // lets prove m1 has inverse of m
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1
##
## > m1$set(matrix(c(4:1),2,2))     // input another matrix into m1
## > m1$get()                       // display it
##      [,1] [,2]
## [1,]    4    2
## [2,]    3    1
##
## > m1$getinverse()                // no inverse yet
## NULL
##
## > cacheSolve(m1)                 // get its inverse
##      [,1] [,2]
## [1,] -0.5    1
## [2,]  1.5   -2
##
## > m1$getinverse()                // display it
##      [,1] [,2]
## [1,] -0.5    1
## [2,]  1.5   -2
##
## > cacheSolve(m1)                 // uses cached new inverse now
## getting cached inverse
##      [,1] [,2]
## [1,] -0.5    1
## [2,]  1.5   -2
##
## > m1$get() %*% m1$getinverse()  // lets prove this works too
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1
##------------------------------------------------------------------

## makeCacheMatrix takes an invertible matrix as input and stores
## its value in a special matrix variable. When this special matrix
## variable is inputted to cachSolve it will acquire the inverse
## of the original matrix and store it within its structure for use.
## It outputs a list of set and get functions to access the matrices.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,       
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve takes a special matrix variable created by
## makeCacheMatrix and returns a stored copy of its inverse if it
## exists, or creates, stores, and returns the inverse otherwise.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached inverse")
                return(inverse)
        }
        matrix  <- x$get()
        inverse <- solve(matrix, ...)
        x$setinverse(inverse)
        inverse
}
