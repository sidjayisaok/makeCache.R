makeCacheMatrix = function(x = matrix()) {
  m = NULL
  if(det(x)==0){
    print("Matrix determinant must not equal zero.")
    return()
  }
  set = function(y) {
    x <<- y
    m <<- NULL
  }
  get = function() x
  setsolve = function(solve) m <<- solve
  getsolve = function() x
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

cacheSolve = function(x, ...) {
  m = x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)	
  }
  data = x$get()
  m = solve(data, ...)
  x$setsolve(m)
  m
}