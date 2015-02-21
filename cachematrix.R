# The below function makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(matinv) m <<- matinv
  getinverse <- function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The below function returns the inverse of the matrix by first checking if
# the inverse has already been computed. In case its done it gets the result  
# and does not compute the inverse again. If not, it computes the inverse 
# and sets the value in the cache using the setinverse function.

# The below function assumes that the matrix supplied is always invertible 
# (as per assignment instructions)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data.")
    return(m)
  }
  matrixdata <- x$get()
       ## Computing the inverse of a square matrix with the solve function
  m <- solve(matrixdata)
  x$setinverse(m)
  m
}
