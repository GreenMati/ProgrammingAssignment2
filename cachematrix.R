## This set of two functions allows us to cache the inverse of a matrix in order
## to save some memory. First function creates the matrix object which inverse
## will be stored in cache, second function calls the inverse of the matrix that
## was cached, or if it wasnt, it calculates the inverse and stores it in cache.

## This first function creates a special "matrix" object
## that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {  ##set the value of the matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x   ##get the value of the matrix
  setinv <- function(inv) inv <<- solve   ##set the value of inverse
  getinv <- function() inv  ##get the value of the inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This second function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.
## Otherwise, it calculates the inverse of the matrix and sets the value
## of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()   ## if the inverse wasnt cached,
  inv <- solve(data, ...)  ## we compute it now
  x$setinv(inv)  ## and send it to the cache
  inv
}

