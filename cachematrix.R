## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      inversion <- NULL
      set <- function(y) {
            x <<- y
            inversion <<- NULL
      }
      get <- function() x
      setINV <- function(new_inversion) inversion <<- new_inversion
      getINV <- function() inversion
      list(set = set, get = get, setINV = setINV, getINV = getINV)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inversion <- x$getINV()
      if(!is.null(inversion)) {
            message("getting cached data")
            return(inversion)
      }
      data <- x$get()
      inversion <- inversion(data, ...)
      x$setINV(inversion)
      inversion
}
