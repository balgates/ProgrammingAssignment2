## This code file contains a wrapper function titled 'makeCacheMatrix' 
## and a complementary function called 'cacheSolve'.


## makeCacheMatrix wrapper function exposes 4 worker functions which are as follows:
## set() -> to store a matrix 
## get() -> to return the stored matrix
## setinverse() -> to store a matrix which is the inverse of the given matrix
## getinverse() -> to return the previously stored inverse matrix
## Usage Hint 1 - To initialize a matrix: a <- makeCacheMatrix(rbind(c(1, 2), c(2, 1)))
## Usage Hint 2 - To print the matrix stored currently: a$get()


makeCacheMatrix <- function(x = matrix()) {
  ## initialise the 'inv' variable
  inv <- NULL  
  
  ## change the matrix assigned
  set <- function(y) {  
    x <<- y
    inv <<- NULL
  }
  
  ## return the internally stored matrix
  get <- function() x   
  
  ## assign an inverse matrix
  setinverse <- function(inverse) {  
    inv <<- inverse
  }
  
  ## returns the previously assigned inverse matrix or NULL if no inverse matrix is assigned
  getinverse <- function() inv 
  
  ## exposes the internal functions as named items
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function acts as an interface to the Matrix created using makeCacheMatrix function
## Use this function to compute the inverse of a matrix.
## The first time this function is called, it computes the inverse of the matrix, 
## stores the inverse internally and then returns the inverse matrix.
## During subsequent calls, the inverse is returned from the internal copy, thereby avoid recomputation.

## Usage Hint: cacheSolve(a) where 'a' is a list output of makeCacheMatrix()
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Retrieving inverse matrix from cache")
    return(inv)
  }
  message("Computing inverse of matrix")
  data <- x$get()
  inv <- solve(data) ## Cache is empty. Compute the inverse matrix.
  x$setinverse(inv) ## Store the computed inverse matrix in the cache variable
  inv ## Return the inverse matrix
}
