#Catching the inverse of a Matrix - MPRS - Assignement 2 - Lexical Scoping
#
#Step1: Creation of the function "makeCacheMatrix", that creates a special matrix that can cache its inverse.
#
makeCacheMatrix <- function(x = matrix()) { 
  z <- NULL
  set <- function(y){
    x <<- y
    z <<- NULL
  }
  get <- function()x                              # A function that returns the vector x stored in the main function
  setInverse <- function(inverse) z <<- inverse   # A function that changes the vector stored in the main function
  getInverse <- function() z 
  list(set = set, get = get, 
       setInverse = setInverse,                   # the value of the input is stored in a variable 'z'
       getInverse = getInverse)
}
#
#Step 2: Creation of the function "cacheSolve" that creates the inverted Matrix of 'x'
#
cacheSolve <- function(x, ...) { 
  z <- x$getInverse()
  if(!is.null(z)){                       # If the inverse has already been calculated, cachesolve can retrieve the inverse from the cache
    message("getting cached data")
    return(z) 
  }
  mat <- x$get()                         # If the inverse has not been calculated, data gets the matrix stored with makeCacheMatrix, m calculates the inverse, and x$setmean(z) stores it in the object z in makeCacheMatrix.
  z <- solve(mat,...)
  x$setInverse(z)
  z
}
#
# - End of assignment 2 -
