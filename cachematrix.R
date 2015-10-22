## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
  
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
  
    get <- function() x
    setinversion <- function(solve) i <<- solve
    getinversion <- function() i
  
    list(set = set, get = get, setinversion = setinversion, getinversion = getinversion)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   i <- x$getinversion()
 
   if(!is.null(i)) {
      message("Getting cached data")
      return(i)
  }
  
    data <- x$get()
    i <- solve(data, ...)
    x$setinversion(i)
    i
        
}
