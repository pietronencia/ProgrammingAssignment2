# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# It is a function that list 4 functions.
# The set function applies new vector values; resets stored mean
# Variables r and c gather the dimension values for object m
# The get function returns the variable input in the main function
# The setMatrix and getMatrix functions stores the variable input

makeCacheMatrix <- function(x = matrix(, nrow = 1, ncol = 1)) {
  m <- NULL
  set <- function(y) {              
    x <<- y
    m <<- NULL
  }
  
  dim(m) <- c(r,c)                  
                                
  get <- function() x
  setMatrix <- function(solve) m <<- solve(x) %*% x
  getMatrix <- function() m
  list(set = set, get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)
}

# cacheSolve: This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
# has not changed), then the cachesolve should retrieve the inverse from the cache.
# The if conditional checks to see if the inverse matches stored inverse value
# Variables r and c gather the dimension values for object dim and m


cacheSolve <- function(x, ...) {
  m <- x$getMatrix()
  if(identical(x, x$setMatrix)) {   
      message("getting cached data")
      return(m)  
 
  }
  else {
      r <- nrow(x)                  
      c <- ncol(x)                  
      dim(data) <- c(r,c)
      dim(m) <- c(r,c)
      
      data <- x$get()
      m <- solve(data, ...)
      x$setMatrix(m)
      m
  }
}
