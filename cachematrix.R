# Example usage:
# x <- matrix(rnorm(9), nrow = 3)           // How to create our base matrix x
# cx <- makeCacheMatrix(x)                  // How to create special matrix using function
# cx$get()                                  // How to retrieve our special matrix
# cacheSolve(cx)                            // How to retrieve the inverse (if not exist)
# cacheSolve(cx)                            // How to retrieve the inverse (from cache, because exists)

## Based on makeVector function we can create makeCacheMatrix one
#
# makeCacheMatrix has contains a function to:
# 1. set the value of the matrix (original: vector)
# 2. get the value of the matrix (original: vector)
# 3. set the value of the inverse (original: mean)
# 4. get the value of the inverse (original: mean)

makeCacheMatrix <- function(x = matrix()) {
          inv <- NULL
        
          set <- function(y) {
                  x <<- y
                  inv <<- NULL
          }
          get <- function() x
          setinv <- function(inverse) inv <<- inverse
          getinv <- function() inv
          list(set = set, get = get,
               setinv = setinv,
               getinv = getinv)
}


## Based on cachemean function we can create cacheSolve one
#
# Solve matrix returned by makeCacheMatrix above.
# cacheSolve can retrieve inverse from cache it has already
# been calculated.

cacheSolve <- function(x, ...) {
       inv <- x$getinv()
     
        # check if matrix inverse already exist
       if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
       }
       
       # if above is null, then calculate matrix
       data <- x$get()
       inv <- solve(data, ...)
       
       # set to cache and return an inverse matrix of our matrix x
       x$setinv(inv)
       inv
}
