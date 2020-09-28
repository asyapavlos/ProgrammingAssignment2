## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## There are two functions makeCacheMatrix
## makeCacheMatrix consists of set, get, setinv, getinv
## library(MASS) is used to calculate inverse for both square and non square matrices

library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL       # Initializing inverse as NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
get <- function()x   # Function to get matrix x
setinv <- function(inverse)inv <<- inverse
getinv <- function(){
  inver <- ginv(x)
  inver%*%x          # Function to obtain inverse of the matrix
  }
list(set = set, get = get,
     setinv = setinv,
     getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {  ## Gets cache data
  inv <- x$getinv()
  if(!is.null(inv)){              ## Checking if inverse is NULL
    message ("getting cached data")
    return(inv)
    
  }
  data <- x$get()
  inv <- solve(data, ...)      ## Calculate inverse data
  x$setinv(inv)
  inv                        ## Return a matrix that is the inverse of 'x'     
  
}
