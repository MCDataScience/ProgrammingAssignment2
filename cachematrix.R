# Matrix inversion is usually a costly computation and there may be some benefit to
# caching the inverse of a matrix rather than compute it repeatedly (there are also
# alternatives to matrix inversion that we will not discuss here).
# Your assignment is to write a pair of functions that cache the inverse of a matrix.

# ##
#This function creates a special "matrix" object that can cache its inverse.
#
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
}
get <- function() x
setinverse <- function(solve)m <<- solve
getinverse <- function() m
list(set=set, get=get, 
     setinverse=setinverse, getinverse=getinverse)
}

# ##
# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already 
# been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  #if the inverse has already been calculated
  if(!is.null(m))
  {
    # get it from the cache and skips the computation.
    message("getting cached data")
    return(m)
  }
  
  #otherwise calculate the inverse
  data <- x$get()
  m <- solve(data,...)
  #sets the value of the inverse in the cache through the setinv function
  x$setinverse(m)
  return(m)
  
  }

