## The functions here are to cache the inverse of a matrix

## The makeCacheMatrix function serves as a tool to create a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <- NULL
  }
  get <- function() x                                    # Get the value of the matrix x
  setinv <- function(invvalue) inv <<- invvalue          # Set the value of inverse for x
  getinv <- function() inv                               # Get the value of inverse for x
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve function is to compute the inverse of the special matrix object
## returned by the makeCacheMatrix function above. If the inverse has already been calculated, 
## then this function will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){                                        # Check if the inverse has already been cached
    message("already cached, getting cached data ...")      # If cached, the function will directly output the cached value
    return(inv)
  }
  data = x$get()
  inv = solve(data)                                         # Calculate the value of matrix inverse
  x$setinv(inv)
  inv
}
