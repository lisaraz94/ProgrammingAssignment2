## Computing the inverse of a matrix takes a lot of computational power. The functions below
## address this by creating a matrix object that stores the value of the matrix and caches its inverse.

## This function creates a matrix object that can cache its inverse. This is done 
## when the function sets the value of the matrix, gets the value of the matrix, sets  
## the value of the inverse of the matrix, gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set, get = get, 
       setinv = setinv,
       getinv = getinv)
}
  
## The function returns the inverse of the matrix created above in makeCacheMatrix.
## If the inverse has already been computed then it will return the inverse from the cache.
## Otherwise it computes the inverse and sets the value in the cache using the setinv 
## function specified above. 
  
    cacheSolve <- function(x, ...) {
      i <- x$getinv()
      if(!is.null(i)) {
        message("getting cached data")
        return(i)
      }
      data <- x$get()
      i <- solve(data,...)
      x$setinv(i)
      i
    }
  
