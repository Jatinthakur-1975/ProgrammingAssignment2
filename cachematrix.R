## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Function to create a special matrix object that can cache its inverse
makeCacheMatrix <- function(mat = matrix()) {
  # Initialize the inverse and make a list to store the matrix and its inverse
  inverse <- NULL
  matrix <- mat

  # Function to set the value of the matrix
  set <- function(newValue) {
    matrix <<- newValue
    inverse <<- NULL  # If the matrix changes, invalidate the cache
  }

  # Function to get the value of the matrix
  get <- function() matrix

  # Function to get the inverse of the matrix
  getInverse <- function() inverse

  # Function to set the inverse of the matrix
  setInverse <- function(newInverse) inverse <<- newInverse

  # Return a list of the functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# Function to compute the inverse of the matrix (caching if possible)
cacheSolve <- function(cacheMatrix) {
  # Retrieve the matrix and its inverse from the cache
  matrix <- cacheMatrix$get()
  inverse <- cacheMatrix$getInverse()

  # If the inverse is not in the cache, calculate it and store in the cache
  if (is.null(inverse)) {
    cat("Calculating inverse...\n")
    inverse <- solve(matrix)
    cacheMatrix$setInverse(inverse)
  } else {
    cat("Retrieving cached inverse...\n")
  }

  # Return the inverse
  inverse
}

