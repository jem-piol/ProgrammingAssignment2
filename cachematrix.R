## Functions below are largely based on the makeVector.R and cachemean.R
## source codes that were given in the programming assignment.
## 

## Creates the ff functions: get, set, setInverse, getInverse of a given matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(...) m <<- solve(x)
  getInverse <- function() m
  
  #Assign the functions as an element in a list
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Basically returns the inverse of the last matrix used.
## If there is no cached inverse matrix, then the function
## will compute for it by calling setInverse from makeCacheMatrix
## function above and return it

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
  
  }
