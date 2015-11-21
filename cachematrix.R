
# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {   # set the value of the matrix
      x <<- y
      m <<- NULL
    }
    get <- function() x    # get the value of the matrix
    setSolve <- function(solve) m <<- solve   #set the value of the inverse of a matrix
    getSolve <- function() m  # get the value of the inverse of a matrix
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)  
}



# This function computes the inverse of the special "matrix" returned by makeCacheMatrix
# If the inverse has already been calculated , 
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getSolve()
        if(!is.null(m)) {
          message("getting cached inverse")
          return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setSolve(m)
        m
}
