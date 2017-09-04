## MakeCacheMatrix function accepts a matrix argument and generates a matrix object which has it's inverse value cached

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv 
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## Biggest trouble in solving this assignment was the realization that the output of 1st function is the input for 2nd function.

## CacheSolve function first checks whether the inverse of the argument has been calculated or not. If it is calculated, then it 
## retrieves and displays the value. if not then it computes and caches the value.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  return(inv)
}
