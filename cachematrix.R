## Two functions are defined to cache the inverse of a matrix. First function (makeCacheMatrix) creates a special "matrix" 
## object that can cache its inverse while second function (cacheSolve) computes the inverse of the special "matrix" returned by 
## first function. If the inverse has already been calculated (and the matrix has not changed), then it retrieve the inverse 
## from the cache. Second function also checks whether a matrix is invertible or not. If matrix is invertible, inverse of the 
## matrix will be returned else message "Matrix can not be inverted" will be returned

## makeCacheMatrix function get and set the matrix whose inverse is to be calculated and return all the values as a list

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() {x}
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve function computes and return the inverse of the matrix returned by makeCacheMatrix. It also tells whether a 
## matrix can be convertible or not

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  } 
  if(!isSymmetric(x$get()) || is.singular.matrix(x$get())){
    message("Matrix can not be inverted")    
  } else {
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
  }
}
