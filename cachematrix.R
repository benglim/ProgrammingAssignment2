## Put comments here that give an overall description of what your
## functions do
## Programming Assignment 2 
## Submitted by: benglim@moootech.mooo.com


## This function (makeCacheMatrix) creates a special 
## "matrix" object that can cache its inverse.
## The input to the function is of class matrix
## The function creates 4 functions within the manipulate the stored data
## variables are created within the function to store the values:
## x: the input value
## s: the computed value

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL                              # initialise and reset to NULL when called
  set <- function(y) {                   # 1st function: to initialise input value
    x <<- y                              # store the input value within the scope of the function
    s <<- NULL                           # reset the previously computed value
  }
  get <- function() x                     # 2nd function to retrieve previously stored input
  setSolve <- function(solve) s <<- solve # 3rd function to apply the solve function
                                          # ie, find the inverse matrix
  getSolve <- function() s                # 4th function to get the value in the store
                                          # will be NULL if not previously computed
  list(set = set, get = get,              # name the functions within a list for easier 
       setSolve = setSolve,               # reference
       getSolve = getSolve)
}


## This function checks if the input matrix has been previously computed
## ie, s has a value. If so, it is returned. If not, it will do the computation

cacheSolve <- function(x, ...) {    # the input comes from the result of the makeCacheMatrix
                                    # function

  s <- x$getSolve()                 # call the getsolve function to retrieve previous data
  if(!is.null(s)) {                 # if it is not null, then return it directly and indicate so
    message("getting cached matrix data")
    return(s)
  }
  data <- x$get()                   # if not found, ie, NULL
  s <- solve(data, ...)             # call the compute intensive function solve
  x$setSolve(s)                     # store the computed value for future use
  s                                 #return the value
}
