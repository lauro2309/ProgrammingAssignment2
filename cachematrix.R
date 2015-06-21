##----------------------------------------------------------------------------------------------##
## Belo Horizonte, Minas Gerais, Brazil                                                         ##
## Created by Lauro Paixão                                                                      ##
## These two function (makeCacheMatrix and cacheSolve) were created to work in pars.            ##
## The basic idea is calculate inverse matrix saving time-consuming computations.               ##
## If you already did the inverse matrix instead to do it again it will use                     ##
## the value store in the memory.                                                               ##
##----------------------------------------------------------------------------------------------##

##----------------------------------------------------------------------------------------------##
## This first function is basically a list of functions 
## that you will use when you call cacheSolve
##----------------------------------------------------------------------------------------------##
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL ## This is the variable where the inverse matrix will be stored

##This function will be use on cacheSolve to get the value of the matrix to be calculated.
  get <- function() x

## This function will be use on cacheSolve to set the value of the inverse matrix, 
## if it isn't be calculated yet.
  setinverse <- function(inverse) i <<- inverse

##This function will be use on cacheSolve to get the inverse matrix from the memory.
  getinverse <- function() i

##makeCacheMatrix returns a list with the functions above to be use on cacheSolve function
list( get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##----------------------------------------------------------------------------------------------##
## This second function uses the list created by makeCacheMatrix as a argument to inverses the matrix
## If the inverse was calculated before it will recover the value from the memory, if is not
## it will calculate the inverse.
## As as sad before, these two functions work in pars. The argument of cacheSolve must be
## the list created by makeCacheMatrix to trying to save time-consuming computations.
## If you only want to do the inverse you can use solve().
##----------------------------------------------------------------------------------------------##
cacheSolve <- function(x, ...) {
## Caches last inverse matrix calculated using a function from x. 
## Remember, x must be the list of functions created by makeCacheMatrix
  i <- x$getinverse() 
 
## If there is a inverse store in i it will send a menssage and return this inverse matrix.  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }

## If the inverse wasn't cauculated yet it will be calculated and saved in memory using 
##   x$setinverse(i) and returns the inverse matrix.
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}


