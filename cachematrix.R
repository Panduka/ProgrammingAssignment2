##The first function, makeCacheMatrix creates a special "matrix", 

## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

##The following function calculates the inverse of the special "matrix"
##created with the above function. 
##However, it first checks to see if the inverse has already been 
##calculated. 
##If so, it gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the i taken to var called 
## "bfinverse" and sets the value of the inverse in the cache
## via the setinverse

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data.")
    return(i)
  }
  bfinverse <- x$get()
  i <- solve(bfinverse)
  x$setinverse(i)
  i
}