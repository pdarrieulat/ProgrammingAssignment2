

##The first function, makeCacheMatrix creates a special "vector", 
## which is really a list containing a function to :

# set : set the value of the matrix
# get : get the value of the matrix
# setinv : set the value of the inv of the matrix
# getinv : get the value of the inv of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(data) inv <<- data
  getsolve <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The following function calculates the solve of the special "vector" created 
## with the above function. 
## However, it first checks to see if the solve has already been calculated. 
## If so, it gets the solve from the cache and skips the computation. 
## Otherwise, it calculates the solve of the data and sets the value of the sove in the cache 
## via the setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting inverse from cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
