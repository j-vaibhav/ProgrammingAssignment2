## The two functions below illustrates the functionality of Caching in R. 
## DESCRIPTION OF CACHING (non technical)
        ## Some processes such as calculation of inverse of a large matrix 
        ## is many time computationally expensive (i.e. they take much time 
        ## and resources for its calculation). When a program needs the result
        ## of such an computationally expensive calculation a number of times, 
        ## then it is considered a smarter practice to cache (or store) the 
        ## resulting value of the calculation in some variable, so that we
        ## do not need to calculate it again and again. This smart practice 
        ## is called Caching (in layman's language). 

## This function named as "makeCacheMatrix", takes a matrix as an argument
makeCacheMatrix <- function(x = matrix()) {
        
        # it then initialise a variable with NULL. This variable will later hold
        # the inverse of the matrix x. Hence the name xInverse is given. 
        xInverse <- NULL 
        
        ## Create setter, getter functions for matrix x and xInverse
        
        # Setter for matrix x
        setX <- function(y) {
                # here the value of input argument y is assignment to x
                x <<- y
                
                # it also initialises xInverse to null
                # notice <<- is used to initialise the variable
                xInverse <<- NULL 
        }
        
        # getter for matrix x
        getX <- function() x # return the input matrix
        
        # getter and setter for xInverse
        setXInverse <- function(inverse) xInverse <<- inverse 
        getXInverse <- function() xInverse 
        
        # return a list that contains these getter, and setters functions, 
        # so that we can use it as an argument for the next function 
        # named as cacheSolve (see below)
        list(setX = setX, getX= getX,
             setXInverse = setXInverse,
             getXInverse = getXInverse)
}


## This function should take the list which is returned by makeCacheMatrix
## as its argument
cacheSolve <- function(x, ...) {
        
        # get the inversed matrix from list x and assign it to variable m
        m <- x$getXInverse() 
        
        # It will be null if the inverse is uncalculated
        # remember the first line of makeCacheMatrix "xInverse <- NULL" 
        
        # if the inverse has been calculated then it will returned
        if(!is.null(m)) { 
                message("getting data from cache")
                return(m)       
        }
        
        # however, if inverse is not calculated, then we get the matrix x 
        mat <- x$getX() 
        # and calculate its inverse using solve function
        m <- solve(mat) 
        
        # then we set it to the list x
        x$setXInverse(m) 
        # and return the solved result
        m 
}


## HOW THESE FUNCTIONS CAN BE USED?
#         source("cachematrix.R")
#         yourMatrixCache <- makeCacheMatrix(yourMatrix)
#         yourMatrixInverse <- cacheSolve(yourMatrixCache) 
#         yourMatrixInverse <- cacheSolve(yourMatrixCache) 

## first call of yourMatrixInverse will calculate the inverse and store it as cache
## second call of yourMatrixInverse will return the inverse from cache