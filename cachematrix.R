## Programming Assignment 2 for R Programming on Coursera
## by SRingshandl 04. July 2022

## makeCacheMatrix contains functions which offer the possibility to
## assign a defined matrix to a variable,
## calculate the corresponding inverse of said matrix and
## recover said matrices from the variable

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  setmatrix <- function(y){
    x <<- y
    m <<- NULL
  }
  
  getmatrix <- function(){
    x
  }
  
  setinverse <- function(inv){
    inv <<- solve(inv)
  }
  
  getinverse <- function(){
    inv
  }
  
  list(setmatrix = setmatrix, 
       getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve checks whether the inverse matrix is set within a makeCacheMatrix object.
## If this is not the case it creates the inverse of the set object and 
## sets it as the inverse matrix of that object (also returns it)

cacheSolve <- function(x) {
  
  if(!is.null(x$getinverse())){
    message("getting cached data")
    return(x$getinverse())
  }
  
  x$setinverse(solve(x$getmatrix()))
  x$getinverse()
}

## Example usage
## m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
## mcm <- makeCacheMatrix(m1)
## cacheSolve(mcm) #sets inverse as not in cache
## cacheSolve(mcm) #returns cached inverse matrix
## rm(mcm) #if you want to start from scratch