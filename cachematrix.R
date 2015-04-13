#' Helper Function to create and cache inverse of a matrix.
#' 
#' @param x Input matrix whose inverse needs to be computed.
#' @method set
#' @method get
#' @method getinverse
#' @method setinverse
#' @examples
#' x<-makeCacheMatrix(rnorm(1:9),3,3)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#' Function to check if cached inverse value if present, if yes then return it
#' else compute the inverse using solve() package and store it in cache for future 
#' use.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)){
    print('Caching: Getting inverse from cache')
    return(inv)
  }
  
  matrix <- x$get()
  inv <-solve(matrix)
  x$setinverse(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}
