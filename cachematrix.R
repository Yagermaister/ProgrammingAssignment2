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

cacheSolve <- function(x, ...) {
      inversion <- x$getINV()
      if(!is.null(inversion)) {
            message("getting cached data")
            return(inversion)
      }
      data <- x$get()
      inversion <- solve(data)
      x$setINV(inversion)
      inversion
}
