## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  mI <- NULL
  set <- function(y) {
    x <<- y
    mI <<- NULL
  }
  get <- function() x
  setInv <- function(solve) mI <<- solve
  getInv <- function() mI
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mI <- x$getInv()
  if(!is.null(mI)) {
    message("getting cached Inverse Matrix")
    return(mI)
  }
  data <- x$get()
  mI <- solve(data, ...)
  x$setInv(mI)
  mI
}
