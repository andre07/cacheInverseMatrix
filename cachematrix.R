makeCacheMatrix <- function(X = matrix()) {
  #This function creates a special "matrix" object that can cache its inverse
  M <- NULL
  set <- function(Y) {
    X <<- Y
    M <<- NULL
  }
  get <- function() X
  setinverse <- function(inverse) M <<- inverse
  getinverse <- function() M
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(X, ...) {
  #This function computes the inverse of the special "matrix" returned by makeCacheMatrix function
  M <- X$getinverse()
  if(!is.null(M)) {
    message("getting cached data")
    return(M)
  }
  data <- X$get()
  M <- solve(data, ...)
  X$setinverse(M)
  M
}
