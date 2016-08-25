## MakeCacheMatrix defines four functions set, get, setinv,getinv




makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
## cacheSolve calculate the inverse of the matrix that is created with 
##MakeCacheMatrix.However, it first checks to see if the Inverse has already 
##been calculated. If so, it gets the Inverse from the cache and skips the 
##computation. Otherwise, it calculates the Inverse of the data and sets the 
##value of the Inverse in the cache via the setinv function


cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
