##Put comments here that give an overall description of what your
##function does

##There are two function makeCacheMatrix, Cacheinverse
##makeChacheMatrix has 4 main points. set,get, setinverse, getinverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL               ## 'inv' is being initialized as Null
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x        ## This is the function to get matrix 'x'
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## Write a short comment describing this function
## The Function cache-inverse is used to get the cache data
cacheinverse <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {           ## Checks to see if the inverse is NULL
    message("getting cached data")
    return(inv)               ## returns inverse value
  }
  matrix_to_invert <- x$get()
  inv <- solve(matrix_to_invert, ...) ##calculates inverse value
  x$setinverse(inv)
  inv ##this will return the matrix that is inversed of 'x'
}