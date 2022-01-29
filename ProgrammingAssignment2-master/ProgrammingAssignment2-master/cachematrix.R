#Develops inverse of function in order to loop a function to simplify iterations


makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

#calculations occur at a faster speed through a caching process

cacheSolve <- function(x, ...) {
  nv = x$getinv()
  if (!is.null(inv)){
    return(inv)
  }
  mat.data = x$get()
  inv = solve(mat.data, ...)
  x$setinv(inv)
  
  return(inv)
}
