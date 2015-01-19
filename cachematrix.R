## cacheSolve is a helped function that create a list of 'methods' to cache data
makeCacheMatrix <- function(x = matrix()) {
  inverse_m <- NULL
  set <- function(y) {
    x <<- y
    inverse_m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse_matrix) inverse_m <<- inverse_matrix
  getinverse <- function() inverse_m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve return a matrix that is the inverse of 'x', if solve(x) was already
##            calculated, return cached data.
cacheSolve <- function(x, ...) {
  inverse_m <- x$getinverse()
  if(!is.null(inverse_m)) {
    message("getting cached data")
    return(inverse_m)
  }
  data <- x$get()
  inverse_m <- solve(data)
  x$setinverse(inverse_m)
  inverse_m
}

## T E S T
## http://ltcconline.net/greenl/courses/203/MatricesApps/inverse.htm
## > x <- matrix(c(1, 1, -3, 0, 1, 0, 4, 6, -10), nrow=3, ncol=3)
## > x
## [,1] [,2] [,3]
## [1,]    1    0    4
## [2,]    1    1    6
## [3,]   -3    0  -10
## > cacheSolve(makeCacheMatrix(x))
## [,1] [,2] [,3]
## [1,] -5.0    0 -2.0
## [2,] -4.0    1 -1.0
## [3,]  1.5    0  0.5
## > 