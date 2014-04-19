## The funcitons in this file are to be used in conjuction to cache the results
## of the expensive inverse calculation done on a matrix using solve. By use the makeCacheMatrix
## method, the special matrixed is cached that is capable of storing the inverse. Using the 
## cacheSolve method will first check this cache before calculating the inverse of the makeCacheMatrix.


## This is a special matrix that has an additional function used to get an set the inverse
## of the matrix. The method takes the matrix that you wish to cache the inverse of. 
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This method accepts a makeCacheMatrix special matrix as a parameter and will return the cached inverse 
## of the matrix if it exists or will calculate and cache the matrix if it has not yet 
## been calculated. Any additional parameters passed to this method will be passed to the solve 
## function when trying to calculate the inverse. 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
