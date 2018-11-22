## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix creates a list of 4 functions to set/get value of the matrix
# and set/get value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
 set<- function(y){
   x<<- y
   inv<<- NULL
 }
 get<- function()x
 setinv<- function(inverse) inv<<- inverse
 getinv<- function() inv
 list(set = set,
      get = get,
      setinv = setinv,
      getinv = getinv)
}


## Write a short comment describing this function
# This function first check if inverse for the input matrix is already
# calculated. If yes, inverse of that matrix is returned right away
# Else, inverse of the input matrix is calculated

cacheSolve <- function(x, ...) {
  inv<- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
        ## Return a matrix that is the inverse of 'x'
  matrixtemp<- x$get()
  inv<- solve(matrixtemp,...)
  x$setinv(inv)
  inv
}
