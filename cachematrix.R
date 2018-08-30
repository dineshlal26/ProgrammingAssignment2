

# Function makeCacheMatrix is a returns a list of functions
# The puspose is to store a martix and a cached value of the inverse of the matrix. Contains the below listed functions:
#  -set  the value of a matrix
#  -get  the value of a matrix
#  -cacheInverse get the cahced value (inverse of the matrix)
#  -getInverse get the cahced value (inverse of the matrix)

# makeCacheMatrix function
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}




# The following function calculates the inverse of a "special" matrix created with 
# makeCacheMatrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
