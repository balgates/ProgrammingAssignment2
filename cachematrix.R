## This code file contains a wrapper function titled 'makeCacheMatrix' 
## and a complementary function called 'cacheSolve'.


## makeCacheMatrix wrapper function exposes 4 worker functions which are as follows:
## 1. set() -> to store a matrix 
## 2. get() -> to return the stored matrix
## 3. setinverse() -> to store a matrix which is the inverse of the given matrix
## 4. getinverse() -> to return the previously stored inverse matrix
## Usage Hint 1 - To initialize a matrix: a <- makeCacheMatrix(rbind(c(1, 2), c(2, 1)))
## Usage Hint 2 - To print the matrix stored currently: a$get()


makeCacheMatrix <- function(x = matrix()) {
  ## initialise the 'inv' matrix as NULL
  inv <- NULL  
  
  ## assign a matrix
  set <- function(y) {  
    x <<- y
    inv <<- NULL ## When a new matrix is assigned, clear off the inverse of previous matrix
    message("Matrix assigned successfully!")
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


## This function acts as an interface to the Matrix created using makeCacheMatrix function.
## Use this function to access the inverse of a matrix.
## This function checks to see if the inverse is already calculated. 
## If so, it returns the previously stored inverse matrix.
## If inverse matrix is not available, the function computes the inverse, 
##    stores it in the 'inv' variable and returns the computed inverse matrix.


## Usage Hint: cacheSolve(a) where 'a' is the list output of makeCacheMatrix()
cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()  ## Find what is currently stored inside the 'inv' variable.
  
  ## If inverse matrix is available, return it
  if(!is.null(inv)) {
    message("Retrieving inverse matrix from cache")
    return(inv)
  }
  
  ## Else, compute the inverse, store it in local variable, and return the value.
  message("Computing inverse of matrix")
  data <- x$get()
  inv <- solve(data) 
  x$setinverse(inv) 
  inv 
}
