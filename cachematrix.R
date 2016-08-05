## cachematrix.R calculates the inverse of a matrix. In order to save computational
## time, it caches the most recent matrix inverse.

## This function clears the cache m if the matrix input is different from
## the matrix that was cached. It also creates a list of named functions that
## set and get the inverse of the input matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve takes in a matrix and checks first to see if the inverse of 
## the matrix has already been cached. If not, it calculates the inverse of
## the matrix and stores it in the cache. ASSUMES MATRIX IS INVERTIBLE!

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
