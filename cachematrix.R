## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
  x <<- y
  im <<- NULL
  }
  get <- function() x
  setim <- function(solve) im <<- solve
  getim <- function() im
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  im <- x$getim()
  if(!is.null(im)) {
        message("getting cached data")
        return(im)
   }
   data <- x$get()
   im <- solve(data, ...)
   x$setim(im)
   im


}

##Example usage:
x<- matrix(c(1,2,3,4),2,2)
xm <-makeCacheMatrix(x)
cacheSolve(xm) #first time run it will recreate inverse
cacheSolve(xm) #second time it will take values from cache

