## makeCacheMatrix will create a list containing 4 functions
## The 4 functions will set the matrix, get the matrix
## set the inverse, and get the inverse
## cacheSolve will obtain the inverse of the matrix from the
## cache if its there, otherwise it will solve for it and
## store the inverse in the cache

## The input z is the matrix that you want to find the
## inverse of. makeCacheMatrix will return a list containing the
## functions mentioned above, as well as the input matrix

makeCacheMatrix <- function(z) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) I <<- Inverse
  getInverse <- function() I
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse, Mat = z)
}


## The input is a variable containing the list returned 
## from makeCacheMatrix
## This will return the inverse of the matrix entered
## into makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Returns the inverse of the matrix contained in x from the cache
  I <- x$getInverse()
  
  ## Determines if the inverse is stored in the cache & returns
  ## the inverse if it is
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  
  ## Set the matrix to be the matrix contained in list x & find
  ## and find the inverse and store it in the cache
  Matrix <- x$set(x$Mat)
  data <- x$get()
  I <- solve(data, ...)
  x$setInverse(I)
  
  ##Return the Inverse Matrix
  I
}
