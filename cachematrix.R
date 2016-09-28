## Coursera R Fundamentals - Week 3 Programming Assignment
## Take cached mean example and convert to use a solve function on a matrix

## The first part creates the components of the function into a list that 
## can be called later by the cacheSolve function.  Step 1 - create a matrix.
## Step 2 - feed matrix into this function and assign it a name.
## NOTE - matrix must be numeric - not integer to be invertible. Cannot use
## matrix(1:16,4,4) to create, must be matrix(rnorm(16),4,4) or a list of numeric 
## values like matrix(c(1.2,5.2,6,10,11,0.5,4,18,9),3,3)

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Step 3 - Call this function on the results of the makeCacheMatrix function.  
## Step 4 - run a 2nd time to confirm that the cached value is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
