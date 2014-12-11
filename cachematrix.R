## The two functions below store a matrix (and cache its inverse) 

## This makeCacheMatrix function creates an object (in the form 
## of list of functions) that can store a matrix and its inverse. The four 
## (member-)functions set the value of the matrix, get the value of the matrix, 
## set the value of inverse and get the value of inverse. 
## The inverse is stored in "invx" variable, and the matrix in "x" variable

makeCacheMatrix <- function(x = matrix()) {
  invx NULL
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  get <- function() x
  setinvx <- function(i) invx <<- i
  getinvx <- function() invx
  list(set = set, get = get,
       setinvx = setinvx,
       getinvx = getinvx)
}


## The cacheSolve functions gets the object created by makeCacheMatrix as argument
## and checks whether it contains already calculated inverse 
## ("invx" returned by getinvx). It either returns the cached invx if it exists, 
## or calculates the inverse, sets the "invx" variable of the object to the 
## calculated value and then returns this value

cacheSolve <- function(x, ...) {
  i <- x$getinvx()
  if(!is.null(i)) {
    message("getting cached data")
    return(invx)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinvx(i)
  i
}
