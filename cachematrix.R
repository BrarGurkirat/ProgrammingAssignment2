# Function to create a special matrix object that caches its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse as NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y  # Assign the matrix to x in the parent environment
    inv <<- NULL  # Reset the cached inverse since the matrix has changed
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  
  # Function to get the cached inverse of the matrix
  getInverse <- function() inv
  
  # Return a list of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

#Function to compute the inverse of the matrix, or retrieve it from cache
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Get the cached inverse
  
  # If the inverse is already cached, return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise, compute the inverse
  mat <- x$get()  # Get the matrix
  inv <- solve(mat, ...)  # Compute the inverse
  x$setInverse(inv)  # Cache the inverse
  
  return(inv)  # Return the inverse
}


