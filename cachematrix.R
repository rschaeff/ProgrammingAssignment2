## This is a short set of functions that demonstrates
## how to use the "<<-" operator to "memoize', or
## cache the results from a time-consuming function
## In this case, we will be using matrix inversion as
## our example

## makeCacheMatrix -- define a set of methods for interacting with a cacheMatrix, such
## that we can get/set the matrix and get/set the inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) { 
   x <<- y
   i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(
    set = set, get = get, setinverse = setinverse, getinverse = getinverse
    )
}


## cacheSolve -- Calculate the inverse of a matrix, using the cached version if possible
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (!is.null(i)) { 
          message("getting cached data")
          return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
        
