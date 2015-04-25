## These functions saves memory and time when solving an inverse of a matrix
## by obtaining the result from the cache when already computed.

## makeCacheMatrix: This function is a list of functions. 
## It creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list( set = set, get = get,
              setinv = setinv,
              getinv = getinv)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrives the inverse from the cache.

cacheSolve <- function(x, ...){
        m <- x$getinv()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
