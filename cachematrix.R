## Put comments here that give an overall description of what your
## functions do

## Function 1: Create a "special" matrix object that can cache its inverse
## by creating a list containing a function 


makeCacheMatrix <- function(x = matrix()) {  
    i <- NULL
    setx <- function(y) {  ## set the value of the matrix 
    x <<- y
    i <<- NULL
}
    getx <- function() x  ##get the value of the matrix
    setinverse <- function(solve) i <<- solve   ## set the value of the inverse
    getinverse <- function() i   ##get the value of the inverse 

    list(setx = setx, getx = getx,   ##create the list
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function 2: Computes the inverse of the special matrix created above; 
## First checks if the inverse has already been calculated; if so, get the 
## inverse from the cache. Otherwise calculate it & set it in the cache

## Get the inverse of the special matrix created above
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  
##Return inverse if already calculated   
  if(!is.null(i)) { 
    message("getting cached data")
    return(i)           
  }
## calculate inverse and set it in cache
  data <- x$getx()
  i <- solve(data,...)
  x$setinverse(i)
  i                    
}
