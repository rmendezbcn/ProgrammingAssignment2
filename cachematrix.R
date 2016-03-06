## Matrix inversion is usually a costly computation and 
##there may be some benefit to caching the inverse of a matrix
##rather than computing it repeatedly

## These two functions cache the inverse of an invertible matrix 

## The first one creates a matrix -in an environment "outside" 
## the environment where the calcualtions are being perfomred-,
## to store the inverse matrix the first time it is calculated. 

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      set_inv_m <- function(solve) m <<- solve
      get_in_vm <- function() m
      list(set = set, get = get,
           set_inv_m = set_inv_m,
           get_in_vm = get_in_vm)
}


## The second function checks if the matrix created above is empty
## and, if not, it calculates the inverse matrix and 
## stores it in the matrix created with the first function.

cacheSolve <- function(x, ...) {
      m <- x$get_in_vm()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get_in_vm()
      m <- solve(data, ...)
      x$set_inv_m(m)
      m
}
