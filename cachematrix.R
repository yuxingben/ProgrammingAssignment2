## write a pair of functions that
## cache the inverse of a matrix.

## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  setM <- function(y){
    x <<- y
    invm <<- NULL
  }
  getM <- function() x
  setinverseM <- function(inverse) invm <<- inverse
  getinverseM <- function () invm
  list(setM=setM, getM=getM, setinverseM=setinverseM, getinverseM=getinverseM)
}



## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
##`cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invm <- x$getinverseM()
  if(!is.null(invm)){
    message("getting cached data")
    return(invm)
  }
  data <- x$getM()
  invm <- solve(data)
  x$setinverseM(invm)
  invm
}

