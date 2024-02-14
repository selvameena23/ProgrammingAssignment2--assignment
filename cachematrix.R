# ## Put comments here that give an overall description of what your
# ## functions do

# ## Write a short comment describing this function

# makeCacheMatrix <- function(x = matrix()) {

# }


# ## Write a short comment describing this function

# cacheSolve <- function(x, ...) {
#         ## Return a matrix that is the inverse of 'x'
# }
makeCacheMatrix <- function(mat = matrix()) {
  # initialize the inverse matrix to NULL
  inv <- NULL
  
  # set function to set the matrix
  set <- function(matrix) {
    mat <<- matrix
    # when the matrix changes, invalidate the cache
    inv <<- NULL
  }
  
  # get function to retrieve the matrix
  get <- function() mat
  
  # set the inverse function
  setInverse <- function(inverse) inv <<- inverse
  
  # get the inverse function
  getInverse <- function() inv
  
  # return a list of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(cacheMatrix, ...) {
  # get the cached inverse
  inv <- cacheMatrix$getInverse()
  
  # if the inverse is already calculated, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # if not, calculate the inverse using solve function
  mat <- cacheMatrix$get()
  inv <- solve(mat, ...)
  
  # cache the calculated inverse
  cacheMatrix$setInverse(inv)
  # return the inverse
  inv
}