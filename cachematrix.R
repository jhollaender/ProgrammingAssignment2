## This function has routines for creating a special matrix which stores it's inverse
## When the inverse function is computed it is stored to save the effort of recomputing it



## This function creates takes a matrix and stores it
## It has 4 operations which work on this matrix
## set and get, change or return the matrix values
## set_inv and get_inv change or return the inverse matrix values 
makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  
  set <- function(y) {
    x <<- y
    inv_x<<- NULL
  }
  
  get <- function() x
  
  set_inv <- function(inv) inv_x <<- inv
  
  get_inv <- function() inv_x
  
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}


## This function checks first wether the invers matrix is already stored and if yes returns it
## Otherwise the inverse matrix is computed and stored in inv_x
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        temp_inv=x$get_inv()
        if(!is.null(temp_inv)) {
          message("getting cached data")
          return(temp_inv)
        }
        x_matrix <- x$get()
        temp_inv <- solve(x_matrix,...)
        x$set_inv(temp_inv)
        temp_inv
}
