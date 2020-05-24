##Peer-graded Assignment: Programming Assignment 2:
##Assignment:Caching the Mean of a Vector
##function to initialize two objects x and m
makeVector <- function(x = numeric()) {
  m <- NULL
##function to set the value of x and  value of m 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
##function to get the value of the vector
  get <- function() x
##function to set the value of the mean
  setmean <- function(mean) m <<- mean
##function attempts to retrieve a mean from the object passed in as the argument
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


##function calculates the mean of the special "vector" 
cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}
##To print the Mean
special_matrix <- makeVector(matrix(1:4, 2, 2))
cachemean(special_matrix)


