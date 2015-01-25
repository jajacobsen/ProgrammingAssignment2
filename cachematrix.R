## This set of functions creates a special object to store
## a matrix and caches its inverse.

## makeCacheMatrix creates a list that defines functions
## to get and set the value of the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL             
        set <- function(y) {  
                x <<- y             
                i <<- NULL
        }
        get <- function() x   
        setinverse <- function(inverse) i <<- inverse   
        getinverse <- function() i  
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function returns an inverse of the input matrix
## If the inverse has already been solved it will be 
## retrieved from the cache, otherwise it will be solved.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
