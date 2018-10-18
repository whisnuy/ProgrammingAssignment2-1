## this scrip for make cache matric
## functions do

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  iv <- NULL
  set <- function(y) {
    x <<- y
    iv <<- NULL
  }
  get <- function() x
  setinvMatrix <- function(inverse) iv <<- inverse
  getinvMatrix <- function() iv
  list(set = set, 
       get = get,
       setinvMatrix = setinvMatrix,
       getinvMatrix = getinvMatrix)
}



## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  iv <- x$getinvMatrix()
  if(!is.null(iv)) {
    message("getting cached data")
    return(iv)  ## Return a matrix that is the inverse of 'x'
  }
  data <- x$get()
  iv <- solve(data, ...)
  x$setinvMatrix(iv)
  iv
}

#cekinv
B <- matrix(c(1,2,3,4),2,2)
B1 <- makeCacheMatrix(B)
B1       
