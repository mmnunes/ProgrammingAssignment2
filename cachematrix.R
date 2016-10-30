##The following two functions (makeCacheMatrix, cacheSolve) are used to cache the inverse of a matrix.

## Function to create a "matrix" that can be cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


## Function to compute the inverse of the "matrix" or retrive the inverse from the cache if the inverse has already been calculated

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)) {
            message("The inverse has already been calculated. Getting Matrix from cache:")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
