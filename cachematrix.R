## Put comments here that give an overall description of what your
## functions do

## Create a "special" matrix object that can cache its inverse
## by creating a list containing a function to (1) set the value of the matrix, 
## (2) get the value of the matrix, set the value of the matrix' inverse; get the
## value of the inverse
 

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    setx <- function(y) {
    x <<- y
    i <<- NULL
  }
    getx <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(setx = setx, getx = getx,
         setinverse = setinverse,
         getinverse = getinverse)
}

matrix <- matrix(c(4,2,7,6),2,2)

mymatrix <- makeCacheMatrix(matrix)


## Function which computes the inverse of the special matrix created above; 
## First checks if the inverse has already been calculated; if so, get the 
## inverse from the cache. Otherwise calculate it & set it in the cache

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$getx()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

cacheSolve(mymatrix)



