## Coursera:    R Programming (Course 2 - Data Science)
## Assignment:  Week 03 Programming Assignment
## File Name:   cashematrix.R
## Programmer:  Clara A. ("clarablara" on GitHub)
## Date:        01/05/2017

## The functions makeCacheMatrix() and cacheSolve() work together to use R lexical scoping to cache 
## matrix-inversing computations.


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { ## initialize x object
    
    i <- NULL ## initialize i object
    
    set <- function(y) {
        x <<- y  ## assign the input argument to the x object in the parent environment
        i <<- NULL  ## assign the value of NULL to the i object in the parent environment
    }
    
    get <- function() x  ## defines the 'getter' for matrix x
    
    setinv <- function(solve) i <<- solve  ## defines the 'setter' for the inverse matrix i
    
    getinv <- function() i  ## defines the getter for the inverse matrix i
    
    list(set = set,  ## assigns each function as an element within a list()
         get = get,  
         setinv = setinv,
         getinv = getinv)
    
}



## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    
    i <- x$getinv()  ## assigns 'getinv' function to i
    
    
    if(!is.null(i)) {  ## checks to see if inverse matrix i is already cached
        message("getting cached data")
        return(i)  ## returns cached inverse matrix i
    }
    
    data <- x$get()
    
    i <- solve(data, ...)  ## calls solve() function to compute inverse matrix, assigns value to i
    
    x$setinv(i)  ## calls the 'setter' for the inverse matrix i
    
    i  ## returns computated inverse matrix i
}