## The main objective of these two functions is to return the inverse of the matrix. However, to rduce
## computational time, it will take the cached value of the inverse rather than recalculate it
## with each iteration. This is useful when working with large datasets.

## This function takes creates a list of a function which get the value of an inverted matrix

makeCacheMatrix <- function(x = matrix())
{
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function checks to see if the matrix has already been inverted (on cache)
## If not, it calculates the inverse
## If already inverted, it outputs the inverted matrix stored in cache

cacheSolve <- function(x, ...) 
{    
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
