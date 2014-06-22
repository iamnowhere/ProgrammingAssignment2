# There are two functions in this script: 
## The first is called makeCacheMatrix and the second is called cacheSolve.
## The makeCacheMatrix funtion creates a matrix object that can cache its inverse
## and the cacheSolve function computes and caches the inverse of the matrix created
## by makeCacheMatrix.

# Funtion 1: makeCacheMatrix

## makeCacheMatrix function creates a cacheable matrix
## makeCacheMatrix can be assigned to a variable such as z
### z<-makeCacheMatrix() assigns makeCacheMatrix function to variable z

## The z object can now be used to define a matrix: 
### z$set(matrix(c(5,7,6,8),2,2))

## This matrix portion of the z object can be retrieved:
### z$get()


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  } 
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


# Funtion 2: cacheSolve

## The cacheSolve function will calculate the inverse of the matrix created 
## with makeCacheMatrix function (Funtion 1, above). 

### The inverse of the matrix created by makeCacheMatrix can be calculated using
### z$get()

## If the value has already been calculated then cacheSolve will
## provide the caluculated version from the cache.

cacheSolve <- function(x=matrix(), ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
          message("getting cached data")
          return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}
