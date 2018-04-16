## Functions to create a special object that stores an invertible matrix and caches it's inverse

## Creates a special "matrix", which is really a list containing a function to:
## 1) Set the value of the matrix
## 2) Get the value of the matrix
## 3) Set the value of the inverse
## 4) Get the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
    
}


## Calculates the inverse of the special "matrix" created in the above function. 
## Checks to see if inverse has already been calculated - if so, gets the inverse from
## cache, otherwise calculates the inverse and sets the value of the inverse in the cache
## via the setsolve function

cacheSolve <- function(x, ...) {

    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
    
}
