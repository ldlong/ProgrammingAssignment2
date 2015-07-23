## makeCacheMatrix and cacheSolve are functions that use the cache feature in R.
## One can store data in the cache using the <<- operator.

## PART 1
## DESCRIPTION OF makeCacheMatrix
## Creates a special matrix that is the placeholder for the inverse of the matrix.


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinserve = getinverse)
}


## PART 2
## DESCRIPTION OF cacheSolve
## The function returns a matrix that is the inverse of 'x'
## It first checks the cache if the inverse has been calculated. If it hasn't, it calculates it. 

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}


