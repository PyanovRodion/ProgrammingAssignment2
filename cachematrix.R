#Functions returning the inverse of the matrix
#If the inverse matrix has been already cached then the function returnes it without recalculation

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ##gets the value of the matrix
  get <- function() x
  ##sets the value of the inverse
  setinverse <- function(solve) m <<- solve
  ##gets the value of the invese
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## prints "getting cached data" if the inverse was cached
  ## prints "calculating" if the inverse was not cached 
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  else {
  #if not previously cached, calculates and caches the inverse
  #then returns the inverted matrix
  	message("calculating")
  	data <- x$get()
  	m <- solve(data, ...)
  	x$setinverse(m)
  	return(m)
  }
}
