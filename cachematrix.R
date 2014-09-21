
#makeCacheMatrix function creates a special matrix with four properties set, get, setInv, getInv.
#input of the function is an empty matrix x
#output of the function is a special matrix with four properties, which is used as input of the cacheSolve() 

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

#cacheSolve() checks if the inverse of matrix was already calculated 
#If it was calculated, the cached output will be returned. Othewise, the inverse of matrix will be calculated.
#x.new is output of makeCacheMatrix() and is an input of cacheSolve()

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
## Steps to run this code
# x.new <- makeCacheMatrix()
# x.new$set(x)
# cacheSolve(x.new)
