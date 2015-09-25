
## R Programming MOOC - second programming assignment
## The following two functions demonstrate how caching values that are computationally expensive to calculate can save time.
## The first function uses closures to store the values of inv and x, and functions to access them, in a matrix object (defined as a list) .
## The second function uses the matrix object to retrieve a cached inverse matrix if avaiable, else calculates and caches a new one. 


## This function creates a "matrix" object that is capable of caching its inverse.  It is made from a list containing four functions:
## set() - sets the value of the matrix and resets the inverse matrix variable.
## get() - returns the value of the matrix
## setinverse() - cache the value of the inverse matrix
## getinverse() - returns the value of the inverse matrix
## The two variables, inv and x, are lexically-scoped, i.e. they are "searched for in the environment in which the function was defined"
## As such, they are only available within the functions defined in the matrix object (also known as closures)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function calculates the inverse of the special "matrix" defined in the function above.
## It will return the cached inverse matrix from the matrix object if it exists, else it calculates the inverse and caches it in the matrix object

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}



