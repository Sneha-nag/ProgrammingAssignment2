## The functions makeCacheMatrix and cacheSolve are using to determine the inverse of a
## matrix in an efficient way by using caching of variables.


## The function makeCacheMatrix is used to return a set of functions applied on a matrix.
## The setinvmatrix is defined to set the input in the cache whereas the getinvmatrix is defined 
## to retrieve the value from the cache.

makeCacheMatrix <- function(x = as.matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvmatrix <- function(invmatrix) m <<- invmatrix
  getinvmatrix <- function() m
  list(set = set, get = get,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)
}


## The function cacheSolve takes a matrix as an argument and first checks if the inverse of
## of it is available in the cache by calling the getinvmatrix function. If the return value is
## not null then the value from the cache is returned. Else, the function solve is 
## called to get the inverse of the matrix. This value is then stored in the cache by
## calling the function setinvmatrix.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinvmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinvmatrix(m)
  m
}

