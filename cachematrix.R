## This function creates a special "matrix" object that can cache its inverse.

## set the input x as a matrix
## set the solved value "d" as a null

makeCacheMatrix <- function(x = matrix()) {
  d<-NULL
  set<-function(y){
    x<<-y
    d<<-NULL
  }
  get <- function()x
  setInverse <- function(inverse) d <<- inverse
  getInverse <- function() d 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  d <- x$getInverse()
  if(!is.null(d)){
    message("getting cached data")
    return(d)
  }
  mat <- x$get()
  d <- solve(mat,...)
  x$setInverse(d)
  d
}
