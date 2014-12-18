

##The first function, makeCacheMatrix creates a special "vector", 
## which is really a list containing a function to :

# set : set the value of the matrix
# get : get the value of the matrix
# setsolve : set the value of the solve of the matrix
# getsolve : get the value of the solve of the matrix

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) cache <<- solve
  getsolve <- function() cache
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The following function calculates the solve of the special "vector" created 
## with the above function. 
## However, it first checks to see if the solve has already been calculated. 
## If so, it gets the solve from the cache and skips the computation. 
## Otherwise, it calculates the solve of the data and sets the value of the sove in the cache 
## via the setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  val_solve <- x$getsolve()
  if(!is.null(val_solve)) {
    message("getting cached data")
    return(val_solve)
  }
  data <- x$get()
  val_solve <- solve(data, ...)
  x$setsolve(val_solve)
  val_solve
}
