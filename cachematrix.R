# Function 1: create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  
  # set the matrix value
  set <- function(y) {
    x <<- y
    a <<- NULL
  }
  
  # get the matrix value
  get <- function() x
  
  # set the inverse value
  setInv <- function(inv) a <<- inv
  
  # get the inverse value
  getInv <- function() a
  
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


# Function 2: calculate the inverse of a matrix and cache the result
cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  
  # if the inverse is already cached, return the cached value
  if (!is.null(inv)) {
    message("getting cached data!")
    return(inv)
  }
  
  # if the inverse is not cached, calculate it and cache the result
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}


# Testing the functions
test <- makeCacheMatrix(matrix(1:4, 2, 2)) # create a matrix object
test$get() # get the matrix
test$getInv() # get the inverse (should be NULL)
cacheSolve(test) # calculate the inverse and cache the result
test$getInv() # get the inverse again (should not be NULL anymore)
