## This functions cache the inverse of a matrix
##1 makeCacheMatrix is a function that creates a special 
## matrix object that can cache its inverse.
##2 cacheSolve is a function computes the inverse of the 
##special matrix returned by makeCacheMatrix above 

##FIRST Computing the inverse of a square matrix can be done with the solve function 
## if X is a square invertible matrix, then solve(X) returns its inverse.
## makeCacheMatrix creates a special matrix, which is
##really a list containing a function to 
##1 set the value of the matrix
##2 get the value of the matrix 
##3 set the value of the inverse
##4 get the value of the inverse

makeCacheMatrix <- function(x = matrix()) { #empty matrix
        mi <- NULL
        set <- function(y) { #set the value of the matrix
                x <<- y #sets function an assigns argument to x
                mi <<-NULL
        }
        get <- function() x
        setminv <- function(solve) mi <<- solve
        getminv <- function() mi
        #create a list of the functions
        list(set = set, get = get, setminv = setminv, getminv = getminv)
}


##SECOND If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { # Return a matrix that is the inverse of 'x'
        mi <- x$getminv() #retrieves the most recent value for matrix inverse
        if(!is.null(mi)){
                message("getting cached data")
                return(mi) #if the value of matrix inverse was previously calculated
                #the function returns the value
        }
        #if was not previously calculated (NULL), then calculates it
        data <- x$get()
        mi <- solve(data, ...)
        x$setminv(mi)
        mi #return the value
}