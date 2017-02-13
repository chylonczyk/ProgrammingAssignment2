## Given an invertible matrix, the code will calculate 
## the inverse matrix or it gets it from the cache.

## Function 'makeCacheMatrix' creates a special 'matrix' object that can cache its inverse. It is made from 4 functions: set, get, setmean, getmean.
## 1 get - returns the matrix x stored in the main function.
## 2 set - changes the matrix stored in the main function.
## 3 setinverse and getinverse set and get, respectively, the value of the inverse as a variable m.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function 'cacheSolve' computes the matrix being the input of cachemean returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache. If the inverse has not been calculated, data gets the matrix stored with makeCacheMatrix, m calculates the inverse, and x$setmean(m) stores it in the object m in makeCacheMatrix.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
