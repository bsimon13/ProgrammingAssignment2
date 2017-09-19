##These functions will compute the inverse of a matrix, and use cached
## outputs to speed up the computation where possible

## makeCacheMatrix creates a list containing functions to:

##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  
      i <- NULL
      set <- function(y) {
        x <<- y
        i <<- NULL
        
      }
      
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }



## Calculates the inverse of the matrix
## If the matrix has already been computed it will give the 
## output straight from the cache
## otherwise it will compute the inverse and set the value of the inverse
## via the setinverse function

cacheSolve <- function(x, ...) {
  
    i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
  }
  
        ## Return a matrix that is the inverse of 'x
