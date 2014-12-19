## Put comments here that give an overall description of what your
## functions do


# caches inverse of matrix x if not yet cached

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL                 # m as NULL matrix
  set <- function(y) {      # set as returned from function(y) {
    x <<- y                 #   set x in new environment as y, y is free variable
    m <<- NULL              #   set m as NULL matrix in new environment 
  }                         

  
  get <- function() x                    # get as returned from function() with x
  setinv <- function(solve) m <<- solve  # return inversed matrix from solve(m) to setinv
  getinv <- function() m                 # pull inversed matrix to getinv
  list(set = set, get = get,             # return list
       setinv = setinv,
       getinv = getinv)
}


## Return a matrix that is the inverse of 'x'


cacheSolve <- function(x, ...) {
  
  m <- x$getinv()					# m as returned from getinv()
  if(!is.null(m)) {					# notify user if m isn't empty matrix
    message("getting cached data")
    return(m)
  }
  data <- x$get()					# data as returned from get()
  m <- solve(data, ...)				# set m as inverse of data matrix
  x$setinv(m)						# call setinv(m)
  m									# return m
}
