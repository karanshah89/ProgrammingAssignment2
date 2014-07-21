## Put comments here that give an overall description of what your
## functions do

## A Matrix in R is just a vector with the dim attribute
## So the Code will be similar for the one with the Vector, just the m has been replaced with inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Steps to Run the Program
## 1) Create a matrix : e.g. c=rbind(c(1, -1/4), c(-1/4, 1)) 
## 2) Make a cache matrix element :  d <- makeCacheMatrix(c)
## 3) Run the function cacheSolve on d to get the inverse: cacheSolve(d): 
## 4) next time it runs, it will go inside the loop

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data) %*% data #This computes the inverse of a matrix
  x$setInverse(inverse)
  inverse
}
