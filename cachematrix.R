## Matrix Caching
## Otakar Andrysek - 10/22/2019 - Intro to R
## These Functions are used to calculate the inverse of a square matrix. 
## Because these calculations can sometimes be quite intense,
## cacheing is used to save and re-use already solved matricies.

# This function creates the cached matrix symbol. The inverse is initially
# set to NULL and is calculated and stored upon initial computation.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set=set, get=get,
       setInv = setInv,
       getInv = getInv)
}

# This function returns a matrix that is the inverse of 'x'. If the
# function is using a cached inverse it will be printed to the console.
cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  print(inv)
  if(!is.null(inv)) {
    message("Getting cashed data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setInv(inv)
  inv
}
#EOF