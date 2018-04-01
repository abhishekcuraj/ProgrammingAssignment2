## This function creates a special "matrix" object that can cache its inverse

## The following functions are used to create a special object that 
##stores a matrix and caches its inverse. The first function, makeCacheMatrix 
##createsa special "matrix", which is really a list containing a function to
##set the value of the matrix##get the value of the matrix
##set the value of the inverse and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set,
       get = get,
       setinv = setinv,       
       getinv = getinv)
}
## This function computes the inverse of the matrix obtained by
#makeCacheMatrix. If the inverse has already been
#calculated (and the matrix has not changed), then cacheSolve should
#retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
} 


#EXAMPLE (testing)
T <- matrix(c(10,7,5,3),2,2)
Y <- makeCacheMatrix(T)
cacheSolve(Y)              #inverse using function
solve(T)                   # inverse using inbuilt command
