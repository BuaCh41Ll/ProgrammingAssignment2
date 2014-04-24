## This code has two functions
## The first function is to cache a matrix
## The second function uses the solve function to find the inverse of the
## cached matrix
## The code is used to reduce the time spent on computing matrix inverses
## It does this by returning the inverse from cache of a matrix which had
## previously been inverted

## makeCacheMatrix follows the course example by setting the 
## the matrix, getting the matrix, setting the inverse, getting the inverse
## it takes a function with argument x and which is a matrix
## the output of the function is a list, its function is to create a 
## "special" matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function undertakes the solve, using solve function, or it 
## returns the inverse which had been computed previously

cacheSolve <- function(x, ...) {
  ## check the cache
  m <- x$getinverse()
  
  ## if the cache is not null then print message and return the inverse from 
  ## the cache
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## if the cache is empty then do the computation
  
  data <- x$get()
  m <- solve(data, ...)
  
  ## set the cache with the result
  
  x$setinverse(m)
  
  ## return the result of the computation
  
  m
  
}
##test solution
##create cached matrix
##mtx <- makeCacheMatrix(matrix(1:4, 2))

##use function to find inverse
##cacheSolve(mtx)

##re-run function to see if will access cache
##cacheSolve(mtx)
