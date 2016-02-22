
makeCacheMatrix <- function(x = matrix()) {     #returns a list containing functions to set, get, set inverse, and get inverse
  m<- NULL                #sets the value of m to NULL
  set <- function(y) {
    x<<- y                #caches the matrix so can check whether it has been solved
    m <<- NULL            #sets value of m to NULL
  }
  get <- function() x
  setinverse <- function(solve) m<<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}                         #creates a list to hold the above 4 functions

cacheSolve <- function(x, ...) {
  m <- x$getinverse()     #if inverse already calculated then get it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }                       #otherwise calculate the inverse
  data <- x$get()
  m <- solve(data, ...) 
  x$setinverse(m)         #inversing the matrix
  m                       #returning the inversed matrix
}
