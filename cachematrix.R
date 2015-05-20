## This is a short set of functions that demonstrates
## how to use the "<<-" operator to "memoize', or
## cache the results from a time-consuming function
## In this case, we will be using matrix inversion as
## our example

## makeCacheMatrix -- define a set of methods for interacting with a cacheMatrix, such
## that we can get/set the matrix and get/set the inverse.
makeCacheMatrix <- function(x = matrix()) {
  #set internal inverse to null
  i <- NULL
  #define "set", a function that sets a matrix x, also nulls the inverse
  set <- function(y) { 
   x <<- y
   i <<- NULL
  }
  #define "get", a function that returns a matrix x
  get <- function() x
  #define "setinverse", a function that sets the local inverse matrix
  setinverse <- function(inverse) i <<- inverse
  #define "getinverse", a function that returns the local inverse matrix
  getinverse <- function() i
  #define the return list of functions
  list(
    set = set, get = get, setinverse = setinverse, getinverse = getinverse
    )
}


## cacheSolve -- Calculate the inverse of a matrix, using the cached version if possible
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        #if x has a cached inverse, use it
        if (!is.null(i)) { 
          message("getting cached data")
          return(i)
        }
        #otherwise, get the data in x
        data <- x$get()
        #use that data to calculate an inverse i
        i <- solve(data, ...)
        #set the inverse cache on x
        x$setinverse(i)
        #return the inverse
        i
}
        
