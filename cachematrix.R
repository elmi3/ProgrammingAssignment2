## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  # Initialize the cached inverse as NULL
  
  set <- function(y) {
    x <<- y   # Set the matrix
    m <<- NULL # Reset the cached inverse
  }
  
  get <- function() x             # Get the matrix
  setinverse <- function(inverse) m <<- inverse # Cache the inverse
  getinverse <- function() m       # Retrieve the cached inverse
  
  # Return a list of functions to interact with the cached matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), it retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()             # Try to retrieve the cached inverse
  if(!is.null(m)) {               # If cached inverse exists
    message("getting cached data") # Print message indicating cached data
    return(m)                     # Return the cached inverse
  }
  
  data <- x$get()                 # Get the matrix
  m <- solve(data, ...)           # Calculate the inverse
  x$setinverse(m)                 # Cache the inverse
  m                               # Return the inverse
}
