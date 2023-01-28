##first i created a special vector that set the value 
##of and got the value of the vector and set the
##value and got the value of the mean then the cachemean function 
##calculates the the mean of the data and sets the value of the mean


##the makeVector creates a special vector which sets
##and gets the value of the vector and mean

makeCacheMatrix <- function(x = matrix()){
  r <- NULL
  set <- function(y){
    x <<- y
    r <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) r <<- inverse
  getInverse <- function()r
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}

## the cacheSolve calculates the mean 

cacheSolve <- function(x, ...){
  ## Return a matrix that is the inverse of 'x'
  r <- x$getInverse()
  if(!is.null(r)){
    message("getting cached data")
    return(r)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(r)
  r
}
