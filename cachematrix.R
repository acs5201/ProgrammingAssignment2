## makeCacheMatrix creates a special "matrix" that will cache the inverse of the matrix.
## cacheSolve will compute the inverse of the special "matrix" created by makeCacheMatrix 
## and if the inverse has already been calculated and the matrix unchanged it will retrieve the inverse directly from the cache.

makeCacheMatrix <- function(x = matrix()) {
     ## This function creates a special "Matrix"
     ## It creates a list containing a function to
     ## 1. set the value of the Matrix
     ## 2. get the value of the Matrix
     ## 3. set the value of the Inverse of the Matrix
     ## 4. get the value of the Inverse of the Matrix
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinv<- function(solve) inv <<- solve
     getinv <- function() inv
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


cacheSolve <- function(x, ...) {
        ## This function returns a matrix that is the inverse of 'x'
        ## If the Inverse has already been calculated and cached and the Matrix unchanged
        ## it will pull the inverse of 'x' from the cache
     
     inv <- x$getinv()
     if(!is.null(inv)) {
          message("getting cached data") 
          return(inv)
     }
     matrixdata <- x$get()
     inv <- solve(matrixdata, ...)
     x$setinv(inv) ## Caches the inverse of the matrix 'x'
     inv  
}
