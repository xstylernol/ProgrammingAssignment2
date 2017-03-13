#Two functions, in conjunction they take a given matrix and calculates its inverse
#Once calculated, the inverse is stored into memory so that the operation does not need to be repeated


# create an object that contains a matrix and also function calls to retrieve/assign the matrix and its inverse
# input matrix must be both invertible and square
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(arg_inv) inv <<- arg_inv
  getinv <- function() inv
  list (set=set, get=get, setinv=setinv, getinv=getinv)
}

# writes the matrix inverse to memory (if not already exists) and returns it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message('getting cached data')
    return(inv)
  }
  inv <- solve(x$get())
  x$setinv(inv)
  inv
}
