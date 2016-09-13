## Put comments here that give an overall description of what your
## functions do

## This fuction is almost an exact replica of the one Roger Peng provided, makeVector
## It returns a list with four functions: set and get functions for the matrix
## As well as setinv and getinv for setting and getting the inverse
## It is just a store for these values, and nothing is actually computed in the function.
## The function names have been changed to reflect matrix inversions as opposed to means

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(mean) m <<- mean
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function is a variation of Roger Peng's cachemean
## except all mean functions are replaced by matrix inversions
## It gets the inverse matrix above, if it is present, it returns it
## Otherwise it computes the matrix inverse, sets it, and returns the inverse
## all get and set functions refer to the makeCacheMatrix function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  ## This is a good place to check for invertibility
  ## TBD if there is time
  ##
  ## This is the one non-cosmetic change to the function:
  ## m <- mean(data, ...) is replaced by
  m <- solve(data)
  x$setinv(m)
  m
}



# Test on matrixes that I can invert
v = c(2,3,2,2)
A = matrix(v, nrow=2, ncol=2, byrow = TRUE)
MCM <- makeCacheMatrix()
MCM$set(A)
MCM$get()
MCM$getinv()
cacheSolve(MCM)
MCM$getinv()
cacheSolve(MCM)

v=c(1, 1, -1, 2)
B = matrix(v, nrow=2, ncol=2, byrow = TRUE)
MCM <- makeCacheMatrix(B)
MCM$get()
MCM$getinv()
cacheSolve(MCM)
MCM$getinv()
cacheSolve(MCM)

# seems to work well!