## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix function creates a special matrix and 
## cache the inverse of the matrix

## Write a short comment describing this function
## the matrix function makeCacheMatrix creates matrix by setting 
## the value and store it in m 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL ## set m to null
        ## change and assign the function of the matrix 
        set <- function(y){
                x <<- y ## assign y function to x
                m <<- NULL # restore m variable to NULL class
        }
        # set the function get with x matrix
        get <- function() x
        # calculate the matrix inverse and store to setmInv
        setmInv <- function(solve) m <<- solve
        # return the value of the inverse matrix to m
        getmInv <- function() m
        #create the list of the function attributes 
        list(set=set, get=get, setmInv=setmInv, getmInv=getmInv)
}

## Write a short comment describing this function
## cacheSolve function will call the matrix stored by makeCacheMatrix
## if the inverse of the matrix is already in the cache memory 
## the cacheSolve function returns the matrix inverse and print 
## the result to the console
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmInv()
        ## Verify if the cache already had the matrix
        if(!is.null(m)) {
                message("getting cached matrix inverse")
                return(m)
        }
        ## if the the matrix is not the inverse, return the stored  
        ## matrix and assign to data
        data <- x$get()
        ## calculate the matrix inverse and assign to m
        m <- solve(data, ...)
        ## set the matrix inverse to m
        x$setmInv(m)
        ## return m to the console
        m 
}
