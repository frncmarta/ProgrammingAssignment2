## The following functions are used to create a special "matrix" and caches it's inverse


## Function makeCacheMatrix creates a list of functions:
## set - that sets the value of a matrix 
## get - that gets the value of a matrix
## setinverse - sets the value of the inverse matrix
## getinverse - gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
   
    ##set inverse matrix as NULL
    s <- NULL        
    
    ##definition of set function
    set <- function(y) {                        
        x <<- y                                 
        s <<- NULL
    }
    
    ##definition of get function
    get <- function() x
    
    ##definition of setinverse function
    setinverse <- function(inverse) s <<- inverse
    
    ##definition of getinverse funcion 
    getinverse <- function() s
    
    ##list -> value to be returned
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function cacheSolve returns the inverse of special "matrix"

cacheSolve <- function(x, ...) {
   
    ## assign value of getinverse function to s
    s <- x$getinverse()
    
    ## if s is not null return cached value for inverse matrix and stop the function
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    
    ## get value of matrix
    data <- x$get()
    
    ## calculate inverse matrix
    s <- solve(data, ...)
    
    ## set the value of inverse matrix
    x$setinverse(s)
    
    ##return inverse matrix
    s
}
