## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

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


## This function will check if there is a cached inverse matrix
## and display the cached matrix. If there is no cached data,
## the inverse matrix is computed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getInverse()
      if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setInverse(inv)
      inv
}

##  TESTING:
## Set the matrix
## >m=matrix(1:4,2,2)
## >m1=makeCacheMatrix(m)
## > m1$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## 
## > m1$getInverse()          #No inverse computed yet
## NULL
## 
## Compute the inverse of the matrix
## > cacheSolve(m1)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## Retrieve the inverse from the cache
## > cacheSolve(m1)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## 
## Print the value of inverse
## > m1$getInverse()
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5


