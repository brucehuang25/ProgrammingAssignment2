## The functions here are to cache the inverse of a matrix

## The makeCacheMatrix function serves as a tool to create a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <- NULL
  }
  get <- function() x
  setinv <- function(invvalue) inv <<- invvalue
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve function is to compute the inverse of the special matrix object
## returned by the makeCacheMatrix function above. If the inverse has already been calculated, 
## then this function should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("already cached, getting cached data ...")
    return(inv)
  }
  data = x$get()
  inv = solve(data)
  x$setinv(inv)
  inv
}