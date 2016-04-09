## The following functions caches the inverse of a matrix

## makeCacheMatrix creates a special "matrix" object 
## that can cache its inverse

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


## cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already
## been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv) 
    ## Returns a matrix that is the inverse of 'x'
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv 
  ## Returns a matrix that is the inverse of 'x'
}