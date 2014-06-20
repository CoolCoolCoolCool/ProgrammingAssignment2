## These functions allow you to create a matrix whose inverse 
## will be cached when it is calculated. This will save time 
## if the inverse is needed multiple times. 
## Create a new matrix with m <- makeCacheMatrix(matrix(???))
## After that, set the matrix with m$set() and get the matrix 
## with m$get().
## To set the inverse you must call cacheSolve(m,...) where 
## ... is any additional arguments to solve(m,...)


## Creates a matrix whose inverse will be cached

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(new_matrix) {
    x <<- new_matrix
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(new_inv) inv <<- new_inv
  getinv <- function() return(inv)
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## This is used to set the inverse of a makeCacheMatrix object
## if the matrix has changed since the inverse was created.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message('Getting cached inverse in cacheSolve')
    return(inv)
  }
  message('Calculating and caching inverse in cacheSolve')
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
