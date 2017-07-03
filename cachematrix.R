## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function creates a special "matrix" object 
#that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  r <- NULL
  #1. set the value of the matrix
  set <- function(y) {
    x <<- y
    r <<- NULL
  }
  
  #2. get the value of the matrix
  get <- function() x
  
  #3. set the value of the inverse
  setinverse <- function(inverse) r <<- inverse
  
  #4. get the value of the mean
  getinverse <- function() r
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
#This function computes the inverse of the special
#"matrix" returned by `makeCacheMatrix` above. If the inverse has
#already been calculated (and the matrix has not changed), then
#`cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  r <- x$getinverse()
  #retrieve the inverse from the cache (if exists)
  if (!is.null(r)){
    message("getting cached data")
    return(r)
  }
  
  #computes the inverse of the matrix
  data <- x$get()
  r <- solve(data, ...)
  x$setinverse(r)
  r
}
