## This function takes a matrix x and applies various functions to do the following:
#1) set the value of the matrix
#2) get the value of the matrix
#3) set the value of the inverse of the matrix
#4) get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inv <- function(solve) inv <<- solve
  get_inv <- function() inv
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}

## This function subsets the first function to get the value of the current inverse.
## It then checks whether or not the inverse value exists or is NULL.
## If NULL, the inverse of the passed matrix will be calculated and set in cache using the subset function "set_inv"

cacheSolve <- function(x, ...) {
        inv <- x$get_inv()
        if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$set_inv(inv)
        inv
}
