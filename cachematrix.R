## Prg assignment #2
## by Igor Kanovsky (ik)


## function input is an inversible matrix
## returns list: the same matrix with set of functions for its cashable inversion

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse )
     
}


## input is a matrix created by makeCacheMatrix()
## returns the inverse matrix wich will be retrived from cash after first calculation

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
   
}
