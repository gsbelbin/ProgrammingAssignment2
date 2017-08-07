## The function makeCacheMatrix takes an invertible matrix and creates an R list object
## containing the matrix which will store the inverse of the matrix the first time it is
## calculated thus removing the requirement to calculate the inverse repeatedly.
##
## The makeCacheMatrix function creates a list containing the matrix x which
## can store its inverse for later use, removing the need to recalculate it
## repeatedly.  An example of its use is as follows:-
##
## mymatrix <- matrix(c(1,2,3,4), nrow=2, ncol=2)
## mycachematrix <- makeCacheMatrix(mymatrix)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(matinv) inv <<- matinv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## The cacheSolve function takes a CacheMatrix created using makeCacheMatrix and
## returns the inverse matrix.  If the inverse has been calculated previously
## then cacheSolve displays a message and returns the stored inverse.  If the
## inverse has not been calculated previously then cacheSolve calculates the
## inverse, stores it for later use and returns it.  An example of its use is
## as follows:-
##
## cacheSolve(mycachematrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}