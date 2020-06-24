## The functions on the script create a list of components which are functions that take
## a matrix as an input to set and get the matrix and the inverse as well as can get 
## the inverse of the matrix from a cache


## This function can create a list of information of a matrix that it takes as a input

makeCacheMatrix <- function(x = matrix()) {
inverse <- NULL
  setmatrix <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  getmatrix <- function () x
  setinverse <- function (z) inverse <<- z
  getinverse <- function () inverse
  list(setmatrix = setmatrix, getmatrix=getmatrix, setinverse=setinverse, getinverse=getinverse)
}


## This function reads the list created on the function above and print the inverse of 
## the matrix if it has already been calculated and if it has not then the function 
## solves it, return the inverse and set the inverse in the list created before

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)){
    message ("getting cached inverted matrix")
    return(inv) 
  } else {
    func <- x$getmatrix()
    inverse <- solve(func)
    x$setinverse(inverse)
    return (inverse)
  }
}
