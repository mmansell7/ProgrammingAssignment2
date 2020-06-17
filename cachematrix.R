## makeCacheMatrix creates a matrix-like object that
## can store a cached inverse of itself. 
## cacheSolve generates the inverse matrix, if it
## has not been solved yet.


makeCacheMatrix <- function(x = matrix()) {
  ## makeCacheMatrix creates a matrix-like object 
  ## (implemented as a list) that
  ## stores a cached inverse of itself. In combination
  ## with cacheSolve, this can save computational
  ## time.
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invmat) inv <<- invmat
  getinv <- function() inv
  list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}



cacheSolve <- function(x, ...) {
  ## Solve and store the inverse of a cacheMatrix, if
  ## it has not already been calculated.  Otherwise,
  ## return the cached inverse.
  inv <- x$getinv()
  if(!is.null(inv)) {
      message("getting cached inverse matrix")
      return(inv)
    }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
