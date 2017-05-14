## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv=NULL
  ##make a matrix out of y, set as x
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  ##get the matrix
  get<- function() x
  ##set inverse by applying solve function
  setinv<- function(solve) inv<<- solve
  ##get inverse
  getinv<- function() inv
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## get inverse value from cache
  inv<-x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  ## get data
  data<-x$get()
  ##calculate inverse
  inv<-solve(data, ...)
  ##set inverse into cache
  x$setinv(inv)
  #print inverse
  inv
  
}