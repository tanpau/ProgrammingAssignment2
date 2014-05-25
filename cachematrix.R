## makeCacheMatrix() creates a special matrix object
## cacheSolve() calculates the inverse of the matrix.

##  makeCacheMatrix() creates a special "matrix", which is really a list containing a function to
##  1. set the matrix
##  2. get the matrix
##  3. set the inverse of the matrix
##  4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x

  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv

  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve() calculates or retrieves inverse matrix of special "matrix" created with makeCacheMatrix()

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  } else {
    inv <- solve(x$get())
    x$setinverse(inv)
    inv
  }
}
