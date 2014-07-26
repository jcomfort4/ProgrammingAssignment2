## makeCacheMatrix creates a "special" matrix that is actually a list of methods/functions that contain information about the matrix
## makeCacheMatrix stores the matrix as well as the inverse of the matrix(once it is computed) and provides users with the ability to
## store/retrieve the inverse of the matrix specified so that it does not have to be computed again.


makeCacheMatrix <- function(x = matrix()) {
  ## set a variable to NULL that will store the inverted matrix.  This must be initialized to NULL so that the
  ## if statement will not be triggered and the inverse will be calculated the first time
  i <- NULL
  ## set initialized the values of the matrix that a user input
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  ## get returns the matrix that was entered for inversion
  get <- function() x
  ## cached inverse is set for i (the variable initialized to NULL to store the inversed matrix)
  setinverse <- function(inverse) i <<- inverse
  ## getinverse returns the inverse of hte matrix if it has already been calculated and returns NULL if it hasn't (NULL triggers else)
  getinverse <- function() i
  
  ## list is returned because makeCacheMatrix is a list of functions that can be used as well as
  ## the environment containing x (the matrix you wish to invert) and i (which is NULL or storing the inverted matrix)
  ## these values must be stored in a separate environment (think cache) so that they can be retrieved and not recalculated.
  list(set = set, get = get, setinverse = setinverse, getinverse= getinverse)
  
}


## cacheSolve uses getinverse to check the "special" matrix and see if there is already an inverse stored in cache.
## If the inverse is not yet stored in cache (NULL) then the function will use solve to compute the inverse and set it in cache ("special" matrix)
##  If the inverse has already been computed and stored in cache, then the function will get the cached inverse and return it.
## Either way cacheSolve will return the inverse of a matrix to users. 
##  NOTE: For this function to work, the matrix entered must already have been made "special" with makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  
  i <- x$getinverse()
  ## if x$getinverse() returns a value then the inverse has already been computed and we will just return this value
  if(!is.null(i)){
    message("getting cached data")
    ##return the inverted matrix
    return(i)
  }
  ##if the inverse has not already been computed then we need to get the matrix and then calculate the inverse 
  ##(this will be the only time that we calculate the inverse)
  
  ##get the matrix that we would like to invert
  data <- x$get()
  ## calculate the inverse
  i <- solve(data, ...)
  ## set the inverse in memory so that we do not need to compute this the next time
  x$setinverse(i)
  ## return the inverted matrix
  i
}
