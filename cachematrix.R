## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    # inv will store the cached inverse matrix
    inv <- NULL
    
    # create the matrix 
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # get the value of the matrix
    
    get <- function() x
    
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    
    # Return the matrix with our newly defined functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    
    # If the inverse is already calculated, return it
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # If the inverse is not yet calculated, so calculate it
    data <- x$get()
    inv <- solve(data, ...)
    
    # Cache the inverse
    x$setinverse(inv)
    
    # Return it
    inv
}
