

makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a special "matrix" object that can cache its inverse
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  setInv <- function(Inverse) Inv <<- Inverse
  getInv <- function() Inv
  list(set=set, get=get,
       setInv=setInv,
       getInv=getInv)

}



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inv <- x$getInv()
  if(!is.null(Inv)){
    message("getting cached data")
    return(Inv)
  }
  matrix <- x$get()
  Inv <- solve(matrix,...)
  x$setInv(Inv)
  Inv
  
}
