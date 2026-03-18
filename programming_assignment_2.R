

makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}

# makeCacheMatrix creates a  list containing a function to 
# 1. Set the value of the matrix 
# 2. Get the value of the matrix 
# 3. Set the value of inverse of the matrix
# 4. Get the value of inverse of the matrix


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


# cacheSolve Compute the inverse of the matrix. 
# If the inverse is calculated before, it returns the cached inverse. 


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
