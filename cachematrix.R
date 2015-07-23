## Pair of functions which allow to cache the inverse of a matrix 
## explicitly write "return" for better lisibility

## Creates a special "matrix" object that can cache its inverse.
## set : set the value of the matrix
## get : get the value of the matrix
## setInverse : set the value of the matrix inverse
## getInverse : get the value of the matrix inverse
makeCacheMatrix <- function(myMatrix = matrix()) {
  
  cachedInverse <- NULL
  
  set <- function(y){
    myMatrix <<- y
    cachedInverse <<- NULL
  }
  
  get <- function(){
    return(myMatrix)
  }
  
  setInverse <- function(inInverse){
    cachedInverse <<- inInverse
  }
  
  getInverse <- function(){
    return(cachedInverse)
  }
  
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
    

}


## Return the inverse matrix for object of type "makeCacheMatrix"
## compute the inverse for the first time
## Get cached inverse after the first compute

cacheSolve <- function(x, ...) {
  matrix <- x$getInverse()
  
  if(!is.null(matrix)) {
    message("getting cached inverse")
    return(matrix)
  }
  
  message("Compute inverse")
  data <- x$get()
  matrix <- solve(data, ...)
  x$setInverse(matrix)
  
  return(matrix)
}
