
##The following two functions create a special matrix, calculates the 
#inverse of the matrix and caches the inverse

##The following function would take in a matrix and return a list 
# with the following functions.
#1.set- sets the data
#2.get- gets the data
#3.setInverse- sets the inverse of the matrix.
#4.getInverse- gets the inverse of the matrix.

makeCacheMatrix <- function(x=matrix()) {
    I <- NULL
    set <- function(y) {
      x <<- y
      I <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) I <<- inverse
    getInverse <- function() I
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}


## The following function takes in the special matrix created by the 
#makeCacheMatrix() and checks if an inverse of the matrix is available 
#and if not calculates the inverse and caches it.

cacheSolve <- function(x, ...) {
  I <- x$getInverse()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, )
  x$setInverse(I)
  I
}
