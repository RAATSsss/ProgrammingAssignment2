## create and cache the invers of a matrix
## example: 
## a<-makeCacheMatrix()
## a$set(matrix(1:4,2,2)
## cacheSolve(a)

## object includes the following 4 functions
## set and get the value of input matrix
## set and get the inverse of input matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invers) inv <<- invers
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## check if the inverse of the input matrix has been calculated before
## if yes then read the cached value to save computation time
## otherwise do the calculation and cache the value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
