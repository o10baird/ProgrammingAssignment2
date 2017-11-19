## 1st Function creates a new "matrix" that is a list of itself or its inverse.
## Second function takes the "matrix list" from the first function, determines if it
## has an inverse yet...
## if it does, then it prints a message and the inverse...
## If it doesn't, then it prints a message, calculates the inverse, then returns that inverse as well.

## This is the function that creates the "matrix list" from a regular matrix.
## Notes included argument by argument as to how I understand its working.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #initial setting of the ivnerse to a null value
  
  # function that can set the various other values called or used
  set <- function(y) {
    x <<- y
    inv <<-NULL
  }
  
  # function that pulls x
  get <- function() x
  
  # function that makes setinverse object equal to the inverse value as
  # well as assigning the original matrix as its inverse.
  setinverse <- function(inverse) inv <<- inverse
  
  # function run that returns the inverse.  Only call if inverse already determined?
  getinverse <- function() inv
  
  # List of objects to be called by cacheSolve
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function takes the "matrix list" and returns either the cached version
## of its inverse, or calculates and returns the inverse on the spot.
## Comments are inserted argument by argument just like the previous function.

cacheSolve <- function(x, ...) {
  
  # sets inv equal to makeCacheMatrix's getinverse value.
  inv <- x$getinverse()
  
  # if the inverse isn't null, then it pulls the cached inverse data
  if(!is.null(inv)) {
    message("Retreiving cached data.")
    return(inv)
  }
  
  # sets an object as the original matrix and calls it data
  data <- x$get()
  
  # function then solves the original matrix, now called data here, and then sets that value as inv.
  inv <- solve(data, ...)
  
  # function then sets the solution in X equal to the solved inverse of the matrix.
  x$setinverse(inv)
  
  # Message and return of inverse if it wasn't already cached.
  message("Geerating inverse...")
  return(inv)
}