## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { 
    INV <- NULL                             # set INV as NULL. this will store the inverse value of the matrix
    set <- function(y) {                    # set the value of the vector
      x <<- y
      INV <<- NULL
    }
    get <- function() x                     # get the value of the vector
    setinverse <- function(inverse) INV <<- inverse # set the inverse value of the matrix
    getinverse <- function() INV                    # get the inverse value of the matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }





## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

## Return a matrix that is the inverse of 'x'
  
cachesolve <- function(x, ...) {
  INV <- x$getinverse()
  if(!is.null(INV)) {                     # check if inverted marix is not NULL
    message("getting cached data")        
    return(INV)                           # return cached matrix
  }
  data <- x$get()                         # if cached value is NULL, get original matrix
  INV <- solve(data, ...)                 # use solve function to get inverted matrix
  x$setinverse(INV)
  INV                                     # return inverted matrix
}