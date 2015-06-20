## Implementation of a Cached matrix
## The implementation allows saving the inversion result to object so that upon repeated calls,  
## if the data hadn't changed, the value from cache will be returned, saving repeat computations. 


## Create a cache matrix object
## input: an inversible matrix
## output: a list of functions for getting and setting the matrix and getting and setting the inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setInv <- function(invert) inv <<- invert
      getInv <- function() inv
      list(set = set, get = get,
           setInv = setInv,
           getInv = getInv)
}


## Inverse and cache a matrix 
## input: an inversible matrix created using makeCacheMatrix
## output: the inverse of the input matrix
## notes: 1. The input matrix must be inversible. 2. the result is cached for efficiency. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getInv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setInv(inv)
      inv
}

