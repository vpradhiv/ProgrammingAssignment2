## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {               #take the matrix as an input
  invMatrix <- NULL

  setMatrix <- function(y) {                              #set the value of the Matrix 
    x <<- y
    invMatrix <<- NULL
  }

  getMatrix <- function() x                              #get the value of the Matrix
  setInverse <- function(inverse) invMatrix <<- inverse  #set the value of the invertible matrix
  getInverse <- function() invMatrix                     #get the value of the invertible matrix
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)

}



cacheSolve <- function(x, ...) {                    ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInverse()                       #get the value of the invertible matrix from the makeCacheMatrix function

  if(!is.null(invMatrix)) {                       #if inverse matrix is not NULL
    message("Getting Cached Invertible Matrix")   #Type message: Getting Cached Invertible Matrix 
    return(invMatrix)                             #return the invertible matrix
  }

                                                  #if value of the invertible matrix is NULL then  
  MatrixData <- x$getMatrix()                     #get the original Matrix Data 
  invMatrix <- solve(MatrixData, ...)             #use solve function to inverse the matrix
  x$setInverse(invMatrix)                         #set the invertible matrix 
  return(invMatrix)                               #return the invertible matrix
}
