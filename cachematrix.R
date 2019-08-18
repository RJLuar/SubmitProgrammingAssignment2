makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL                                         ##initialises by setting the inverse value to NULL
  set <- function(y) {                              
    x <<- y
    i <<- NULL }
  
  get <- function() x
  setinverse <- function(inverse) i <<- inverse     ## set the cache i equal to the inverse of the matrix x
  getinverse <- function() i                        ##returns cached inverse
  list(set = set, get = get,                        ##creates a list that the "cacheSolve" function refers to
       setinverse = setinverse, 
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {                               ##if "getinverse" is not null, retrieves that instead of
    message("getting cached data")                 ##recalculating the inverse using "solve"
    return(i)
  }
  mat <- x$get()                                   ##retrieves the un-inversed matrix from the list
  i <- solve(mat, ...)                             ##calculates the inverse using the "solve" function
  x$setinverse(i)                                  ##changes the setinverse item on the list to the inversed matrix
  i                                                ##prints the inverted matrix
}