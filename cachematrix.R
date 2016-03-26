## This function creates a matrix, which is a list containing functions to 
#  1. set the value of the matrix
#  2. get the value of the matrix
#  3. set the value of the invers
#  4. get the value of the invers

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y){
     x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv 
  list(set=set, get=get, 
       setinv=setinv,
       getinv=getinv)
}


## This function calculates the invers of the matrix created 
## with the above function. It first checks to see if 
## the invers has already been calculated.
## If so, it gets the invers from the cache and skips the 
## computation. Otherwise, it calculates the invers of the data 
## and sets the value of the invers in the cache via the setinv 
## function.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  
}
