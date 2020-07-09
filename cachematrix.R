
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinvers <- function(invers) inv <<- invers
  getinvers <- function() inv
  list(set = set, get = get,
       setinvers = setinvers,
       getinvers = getinvers)
}

cacheSolve <- function(x) {
  inv <- x$getinvers()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinvers(inv)
  inv
}
