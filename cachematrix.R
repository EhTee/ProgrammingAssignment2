

## The makeCacheMatrix function will create and return a list
## of functions used by cacheSolve to get or set the inverted 
## matrix in cache

## makeCacheMatrix will create a matrix in the working environment 
## and invert the value of said matrix and store in cache and will 
## return the functions to the working environent
makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y) {
    x <<- y 
    cache <<- NULL
  }
  get <- function() x
  setmatrix <- function(inverse) cache <<- inverse
  getinverse <- function() cache
  list(set = set, get = get, 
       setmatrix = setmatrix, 
       getinverse = getinverse)
}



## cacheSolve will calculate the inverse of the matrix created in
## makeCacheMatrix, or it will create a matrix in the working 
## environment and store the inverted value in cache  

cacheSolve <- function(x, ...) {
  cache <- x$getinverse()
  if(!is.null(cache)) {
    message("getting cached data")
    return(cache)
  }
  matrix <- x$get()
  tryCatch( {
    cache <- solve(matrix, ...)
  },
  error = function(e) {
    message("Error:")
    message(e)
    return(NA)
  }, 
  finally = {
    x$setmatrix(cache)
  } )
  return(cache)
}

