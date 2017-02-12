## Put comments here that give an overall description of what your
## functions do

## The function below caches the inverse of a matrix using list, get, set, etc
## Set the value of matrix and its inverse
## Get the value of matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(a) {
    x <<- a
    inv <<- NULL
  }
  get <- function() x
  setInversed <- function(inverse) inv <<- inverse
  getInversed <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}



## This function below calculates the inverse using the above matrix
## There's error handling to skip calculation otherwise calculates the retrieved data

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInversed()
  if (!is.null(inv)) {
    message("retrieve data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInversed(inv)
  inv
}
