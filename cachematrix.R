## The functions here are an optimisation for the use of the "solve" function for
## inverting matrices. 

## The typical use of solve for matrix inversion is illustrated below:

## example_matrix=rbind(c(1, 2, 3), c(3, 2, 1), c(2, 1, 3))
## solve(example_matrix)

##       [,1]        [ ,2]     [,3]
## [1,] -0.41666667  0.25  0.3333333
## [2,]  0.58333333  0.25 -0.6666667
## [3,]  0.08333333 -0.25  0.3333333

## What we want to do here is logically equivalent to this example, 
## but which behaves like tis:

## cache_matrix <- makeCacheMatrix(rbind(c(1, 2, 3), c(3, 2, 1), c(2, 1, 3)))
## cache_matrix$getinverse()
## NULL
## cacheSolve(cache_matrix)
##       [,1]        [ ,2]     [,3]
## [1,] -0.41666667  0.25  0.3333333
## [2,]  0.58333333  0.25 -0.6666667
## [3,]  0.08333333 -0.25  0.3333333
## cacheSolve(cache_matrix)
## Getting cached data.
##       [,1]        [ ,2]     [,3]
## [1,] -0.41666667  0.25  0.3333333
## [2,]  0.58333333  0.25 -0.6666667
## [3,]  0.08333333 -0.25  0.3333333
## cache_matrix$getinverse()
##       [,1]        [ ,2]     [,3]
## [1,] -0.41666667  0.25  0.3333333
## [2,]  0.58333333  0.25 -0.6666667
## [3,]  0.08333333 -0.25  0.3333333
## inverted_matrix <- cacheSolve(cache_matrix)
## Getting cached data.
## inverted_matrix
##       [,1]        [ ,2]     [,3]
## [1,] -0.41666667  0.25  0.3333333
## [2,]  0.58333333  0.25 -0.6666667
## [3,]  0.08333333 -0.25  0.3333333
## cache_matrix$set(rbind(c(2, 1, 3), c(1, 2, 3), c(3, 2, 1)))
## cache_matrix$get()
##        [,1]  [,2] [,3]
## [1,]    2    1    3
## [2,]    1    2    3
## [3,]    3    2    1
## cacheSolve(cache_matrix)
##      [,1]      [,2]         [,3]
##[1,]  0.3333333 -0.41666667  0.25
##[2,] -0.6666667  0.58333333  0.25
##[3,]  0.3333333  0.08333333 -0.25
## cacheSolve(cache_matrix)
## Getting cached data.
##       [,1]       [,2]        [,3]
## [1,]  0.3333333 -0.41666667  0.25
## [2,] -0.6666667  0.58333333  0.25
## [3,]  0.3333333  0.08333333 -0.25

## Creator function which creates a matrix with caching behaviour. 
## See above for usage.

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Function that solves, i.e. creates the inverse, of a matrix
## created by the makeCacheMatrix function.
## See above for usage.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
