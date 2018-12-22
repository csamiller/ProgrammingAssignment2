## function that allows the values of the matrix and its inverse to be stored

makeCacheMatrix <- function(x = matrix()) {
  
    inverse_matrix <- NULL
    set <- function(y) {
      x <<- y
      inverse_matrix <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverse_matrix <<- inverse
    getinverse <- function() inverse_matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  
}

## function that retrieves the value of the inverse or calculates it if not in the cache

cacheSolve <- function(x, ...) {
  
    inverse_matrix <- x$getinverse()
    if(!is.null(inverse_matrix)) {
      message("getting cached data")
      return(inverse_matrix)
    }
    data <- x$get()
    inverse_matrix <- solve(data, ...)
    x$setinverse(inverse_matrix)
    inverse_matrix
  
}
