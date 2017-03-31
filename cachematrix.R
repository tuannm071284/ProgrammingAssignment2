## this function will make a special matrix as a list of functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x #return the matrix which is set by set function
  setinv <- function(inv) m <<- inv #assign the inverse to m
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv) # return a list of functions
}


##this function will compute the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) # check for the existing of inverse
  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)# calculate the inverse matrix
  x$setinv(m)
  m
}
