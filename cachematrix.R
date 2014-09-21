## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  
  setInv <- function(Inverse) inverse <<- Inverse
  getInv <- function() inverse
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)  
}

## Write a short comment describing this function

cacheSolve <- function(x.new, ...) {
  ## Return a matrix that is the inverse of 'x.new'
  inverse <- x.new$getInv()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x.new$get()
  inverse <- solve(data, ...)
  x.new$setInv(inverse)
  message("inverse not available in cache")
  inverse
}
## How to run this code
# x.new <- makeCacheMatrix()
# x.new$set(x)
# cacheSolve(x.new)
