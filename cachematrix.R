## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function that caches the "matrix" object 
## and can cache its inverse for the input. Default is an empty matrix.

## Arguments: 
## "x" is a matrix that you pass into the function
## "inv" is the variable that will store the inverse matrix once caculated
## (set to NULL unless set via the "setinverse" argument)
## "set" allows you to set the value of the matrix
## "get" allows you to call the value of the matrix
## "setinverse" allows you pass the inverted maxtrix as a value and is 
## stored in the function
## "getinverse" allows you to call the inverse of the matrix

## Output is a list of functions that you can use to call the functions

makeCacheMatrix <- function(x = matrix()) { 
      
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set, get = get, setinverse = setinverse, 
           getinverse = getinverse)
}


## CachdSolve is a function that calculates the inverse of the "matrix" object 
## from makeCacheMatrix. If the inverse has already been calculated (and the 
## matrix not changed), then cachSolve should retrieve the inverse from the cache.

## Arguments: 
## "inv" is the argmuent for storing the inverted matrix - first occurance in the
## function is to determine if the inv argument is already NULL.
## -if NULL- 
## "data" is original matrix from makeCacheMatrix
## "inv" is used to store the inverted matrix

## CacheSolve will put the inverse in the makeCacheMatrix variable but will 
## not have a return value.


cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting chached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
}


## __________________TEST_____________________
## matrix1 <-  matrix(rnorm(9), 3, 3)

## Cached_MATRIX <- makeCacheMatrix(matrix1)

## > Cached_MATRIX$get()
## [,1]       [,2]       [,3]
## [1,]  1.1991056 -0.7731733 -0.6693063
## [2,] -0.2548782  0.9835220 -1.0268776
## [3,]  0.3310646 -1.2966591 -1.1663805

## cacheSolve(Cached_MATRIX)

## > Cached_MATRIX$getinverse()
## [,1]        [,2]       [,3]
## [1,]  0.998356217  0.01367516 -0.5849282
## [2,]  0.256669903  0.47408263 -0.5646662
## [3,] -0.001965892 -0.52315365 -0.3956423
