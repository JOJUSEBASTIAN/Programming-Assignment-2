##Peer-graded Assignment: Programming Assignment 2:
##Assignment: Caching the Inverse of a Matrix
##function below set/get matrix and set/get inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
## Method the get the matrix
  get <- function() x
## Way to set the inverse of the matrix
  setinverse <- function(inverse) i <<- inverse
## Way to get the inverse of the matrix
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## function below return  the inverse of a matrix
cacheSolve <- function(x, ...) {
##Set the inverse  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("cached inverse matrix found, getting the matrix...")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
##To print the inverse of a matrix
inverse_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
cacheSolve(inverse_matrix)


