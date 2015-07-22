## makeCacheMatrix creates a "special" matrix that is actually a list of 
## functions or methods for getting and setting a (square) matrix and for
## getting and setting the inverse of a matrix (assumed to be invertable)
## It takes as it's argument a matrix

## cacheSolve takes as it's argument a "special" matrix created with makeCacheMatrix
## and returns the inverse.  The first time cacheSolve is called, the inverse of the 
## matrix is calculated using the R solve() function.  Subsequent calls to cacheSolve
## with the same "special" matrix argument result in the cached inverse being returned

## makeCacheMatrix creates the "special" matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve calculates (or retrieves from cache) the inverse of an invertable matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
