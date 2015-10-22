## makeCacheMatrix and cacheSolve are a pair functions that work together to cache the inverse of a matrix so that 
## repeated computation of the inverse is not needed. 

## makeCacheMatrix creates a special 'matrix' that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
   
    i <- NULL
   
    ## set and cache the matrix value and initialise the cache inverse to NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## functions to get the matrix, compute the inverse and get the inverse
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
  
    ## returns a list of functions to compute the inverse of the matrix
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


##  cacheSolve computes the inverse of the matrix.
##  First the function checks if the inverse is already in cache and if so, retrives the cached value.
##  If a cached value is not available, the function computes the inverse and caches the value.

cacheSolve <- function(x, ...) {

   i <- x$getinverse()
   
   ## check is the inverse is already cached and if so, return the cached value
   if(!is.null(i)) {
      message("Getting cached data")
      return(i)
   }

    ## if the inverse is not in the cache, compute the inverse, cache the value and return the value  
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
        
}
