## This function creates a special "matrix" object that can cache its inverse
## It allows you to:
## 1 set the value of the matrix
## 2 get the value of the matrix
## 3 set the value of the inversed matrix
## 4 get the value of the inversed matrix


makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  #These are basic setters and getters. Howevr on set
  # m is set to Null due to the new inverse not being known.
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  # Set the inversed matrix, making it available an object in an environment 
  # that is different from the current environment.
  setInversedMatrix <- function(inversedMatrix) m <<- inversedMatrix
  getInversedMatrix <- function() m
  
  # I usethe function list() to store the 4 functions in the function makeCacheMatrix, 
  # we need, so that when we assign makeCacheMatrix to an object, 
  # the object has all the 4 functions.
  list(set = set, get = get,
       setInversedMatrix = setInversedMatrix,
       getInversedMatrix = getInversedMatrix)
}


## The following function calculates the inverse of the special "matrix" 
## created with the above function (makeCacheMatrix). However, it first 
## checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of 
## the inverse in the cache via the setInversedMatrix function.

cacheSolve <- function(x, ...) {
  
  cm <- x$getInversedMatrix()
  
  if(!is.null(cm)) {
    message("getting cached data")
    return(cm)
  }
  
  data <- x$get()
  cm <- solve(data, ...)
  x$setInversedMatrix(cm)
  
  ## Return a matrix that is the inverse of 'x'
  cm
}
