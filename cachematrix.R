## A program that creates a special matrix, caches the inverse and returns inverse from cache if it has already been calculated 
## @tkaranja 06.04.2017

## This function creates a special "matrix" object and caches its inverse 
makeCacheMatrix <- function(x = matrix()) {
      # Set the matrix
      invMat <- NULL
      set <- function(y){
            x <<- y
            invMat <<- NULL            
      }
      
      # Get the matrix
      get <- function() x
      
      # Set the inverse of the matrix
      setInverse <- function(solve) invMat <<- solve
      
      # Get the inverse of the matrix      
      getInverse <- function() invMat
      list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
}


## This function computes the inverse of a special "matrix" and returns the inverse from the cache...
## ...if the inverse had already been calculated

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      invMat <- x$getInverse()
      if(!is.null(invMat)){
            message("getting cached data")
            return(invMat)
      }
      data <- x$get()
      invMat <- solve(data, ...)
      x$setInverse(invMat)
      invMat
}

# Test data
m <- matrix(c(1:4), 2, 2)
c <- makeCacheMatrix(m)
cacheSolve(c)
