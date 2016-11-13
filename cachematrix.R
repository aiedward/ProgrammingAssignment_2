
# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

## makeCacheMatrix will create a matrix x, and expose three methods to set/get x and its inverse

makeCacheMatrix <- function(x = matrix()) {
  cachedInv <- NULL ## initialize inverse
  
  ## set x in parent env with the desired value, if inverse is already set, get rid of it!
  set <- function(userValue = matrix()) {
    x <<- userValue 
    cachedInv <<- NULL
  }
  
  get <- function() x
  
  ##set inverse variable in parent env to desired value and return the value as a convenience

    setInverse <- function(invVal) {
    cachedInv <<- invVal 
    return(cachedInv)
  }
  
  getInverse  <- function() cachedInv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## given the list variable from the first function, will first check to see if there's already a cached inverse and return
## otherwise will attempt to solve its inverse and set/return it

cacheSolve <- function(x=makeCacheMatrix(1:4, nrow=2, ncol=2), ...) { ##special matrix provided or create a test 2x2 matrix
  
  ## let's see if there's something there already
  calculatedInverse <- x$getInverse() 
  
  ##check if there's a cached value AND it's a matrix
  
  if(!is.null(calculatedInverse) && is.matrix(calculatedInverse)) { 
    message("Getting cached data !!")
    return(calculatedInverse)
  }
  
  ## otherwise get the matrix
  matrixToSolve <- x$get()  
  
  ## try to solve the matrix and catch errors and warnings
  calculatedInverse <- tryCatch({ 
    solve(matrixToSolve)
  }, 
    warning=function(w) {
    message("This may not be the result you're looking for it !!")
    message(w)
    }, 
  
    error=function(e) {
    message("Something went wrong solving your matrix !!")
    message(e)
    message("\n")
  })
  
  ## whatever the case, set the value of the inverse (NULL if something went wrong)
  message("Setting the value of inverse to:") 
  x$setInverse(calculatedInverse)
}

## Sample run:
## 
## > x = rbind(c(1, -1/5), c(-1/5, 1))
## > x
##      [,1] [,2]
## [1,]  1.0 -0.2
## [2,] -0.2  1.0
## > 
## > m = makeCacheMatrix(x)
## > m$get()
##      [,1] [,2]
## [1,]  1.0 -0.2
## [2,] -0.2  1.0
## > 
## > cacheSolve(m)
## Setting the value of inverse to:
##           [,1]      [,2]
## [1,] 1.0416667 0.2083333
## [2,] 0.2083333 1.0416667
## > 
## > cacheSolve(m)
## Getting cached data !!
## [,1]      [,2]
## [1,] 1.0416667 0.2083333
## [2,] 0.2083333 1.0416667
## > 