## Hi I am Angie Perez and this is a function that will get a matrix and cache
 # it on a hidden place so that then this matrix can be inverted and viceversa

## This function is the one that will capture the matrix and chache it 

makeCacheMatrix <- function(x = matrix()) {
  invert <- NULL
  set <- function(y) {
    x <<- y
    invert <<- NULL
  }
  get <- function() x
  setinvert <- function(invertion) invert <<- invertion
  getinvert <- function() invert
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
}


## This function is the one that will invert the matrix 
  # Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  invert <- x$getinvert()
    if(!is.null(invert)) {
      message("getting cached data")
      return(invert)
    }
    data <- x$get()
    invert <- solve(data)
    x$setinvert(invert)
    invert
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}
