## author: Zenith Arnejo

## this function accepts a matrix in put as a parameter and places the matrix in the cache
## also, upon initializing the function to a variable with the matrix as parameter,
## this function can be used in cacheSolve in order to comput for the inverse of the matrix
## once cacheSolve gets executed, the m (or the inverse) will also be placed in the cache
## thus, at any point in time, this function could provide the matrix and its inverse 
## by calling the getter functions
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## this function accepts a list with headers containing getinverse, get, and setinverse (the very result of the makeCacheMatrix function)
## basically, this function first checks whether an inverse is already stored in the cache,
## if there is, the function just returns the inverse
## if there is not, the function gets the matrix stored in the cache by the function stated in the parameter
## after which, using the solve function, the inverse of the matrix is computed
## then stored in the cache as specified by the parameter function using the setinverse function
## afterwhich, the computed inverse is returned 
## Lastly, this function returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m  
}
