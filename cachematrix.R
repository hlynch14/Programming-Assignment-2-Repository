# makeCacheMatrix creates a special "matrix" object that can cache its inverse.
# cacheSolve either retrieves the inverse from the cache or computes the inverse
# of the special "matrix" returned by makeCacheMatrix.

# makeCacheMatrix creates a special "matrix" object that can cache its inverse.
# Call the matrix passed into this function "x".
makeCacheMatrix <- function(x = matrix()) {
  # Initialize inverse to be NULL. This will cause cacheSolve to compute the
  # inverse of the matrix x (assuming it hasn't already been computed).
  inverse <- NULL
  
  # Create a function that allows the user to change the matrix they would like to
  # take the inverse of
  set <- function(y) {
    # Change x to y in the cache
    x <<- y
    
    # Change inverse to NULL in the cache. This will cause cacheSolve to compute
    # the inverse of this new matrix.
    inverse <<- NULL
  }
  
  # Create a function that allows cacheSolve to access the matrix x
  get <- function() { x }
  
  # Create a function that allows cacheSolve to set the inverse matrix in the
  # cache
  set_inverse <- function(inv) { inverse <<- inv }
  
  # Create a function that allows cacheSolve to access the inverse matrix
  get_inverse <- function() { inverse }
  
  # Create the special "matrix" object that is really a list
  list(set = set, get = get, set_inverse = set_inverse,
       get_inverse = get_inverse)
}

# cacheSolve computes the inverse of the special "matrix" returned by
# makeCacheMatrix. If the inverse has already been calculated (and the matrix has
# not been changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  # Return the inverse if it is in the cache
  inv <- x$get_inverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Calculate the inverse if it is not in the cache
  my_matrix <- x$get()
  inv <- solve(my_matrix, ...)
  
  # Set the inverse in the cache & return it
  x$set_inverse(inv)
  inv
}