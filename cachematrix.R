## ASSIGNMENT 2: LEXICAL SCOPING  
## Overall description: Two functions that cache the inverse of a matrix. 
## First, I created a function called makeCacheMatrix() which creates an object 
## that can cache its inverse. Second, I created a function called cachesolve())
## which calculates the inverse of the special "matrix" returned in function 1; 
## if the inverse has already been calculated, then cachesolve() retrieves the 
## inverse from the cache 

## Function 1: Create a "special" matrix object
 
makeCacheMatrix <- function(x = matrix()) {  ## initialize objects
    i <- NULL
    setx <- function(y) {  ## set the value of the matrix 
    x <<- y
    i <<- NULL
}
    getx <- function() x  ##get the value of the matrix
    setinverse <- function(solve) i <<- solve   ## set the value of the inverse
    getinverse <- function() i   ##get the value of the inverse 

    list(setx = setx, getx = getx,   ##create a list containing those 4 functions
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