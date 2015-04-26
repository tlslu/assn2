## The following pair of funcitons will return the inverse of an invertible matrix
## The base assumption is that the matrix input is invertible
## The first function will create a vector of functions that will store results of inverse
## The second function will run through the created function vector, if a value is stored in the vector
##    for the inverse, it will return that value without recalculating the inverse
##    If no inverse is stored, this function will calculate the inverse and store it in the vector
## This will save processing time if you will be repeatedly calling the inverse of the same matrix

## create a vector of functions

makeCacheMatrix <- function(x = matrix()) {
  ##function w/ input of matrix
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  } ## sets x to y and i to NULL in this function and the parent environment
  get <- function() x    ##returns x
  setinverse <- function(solve) i <<- solve ##sets i to inverse using <<-
  getinverse <- function() i    ##function that returns i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) ## returns a list of set, get,setinverse,getinverse
  
}


## the following function takes the vector created in prior function as input
## if no value exists for the inverse of the matrix, then it will calculate the value and store it in the vector
## when this function is called again, it will find the value of the inverse that has been saved
##   without calculating again

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse() 
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }  ##if value for i exists, return a message that it is gettinng cached data and then return i
  else {
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
  } ##otherwise solve for i and set value within vector for future calls
}
