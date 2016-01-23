## makeCacheMatrix: A list of functions to 
## 1. Set the value of mtrix
## 2. Get the value of mtrix
## 3. Set the inverse of mtrix
## 4. Get the inverse of mtrix
makeCacheMatrix <- function(x = matrix()) {
	invm <- NULL
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invm <<- inverse
  getinverse <- function() invm
  list(set = set, get = get,
      setinverse = setinverse,
      getinverse = getinverse)
}

## cacheSolve: Compute the inverse of a matrix. We check 
## first to see if the inverse already exists in the cache, 
## and if it does, we retrieve it.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invm <- x$getinverse()
  if(!is.null(invm)) {
    message("getting cached data")
    return(invm)
  }
  data <- x$get()
  invm <- solve(data, ...)
  x$setinvers(invm)
  invm
}