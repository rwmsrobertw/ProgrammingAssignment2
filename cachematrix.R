## Put comments here that give an overall description of what your
## functions do

## Package Functions
##
## makeCacheMatrix - Creates functions to manipulate a matrix (set the matrix, get the matrix, call code to set or 
##                   get the matrix's inverse.
##
## cacheSolve      - Use the makeCacheMatrix functions to calculate and retireve the matrix inverse.


## makeCacheMatrix - Use inputs to create a set of function to create a matrix, return the matrix,
##                   calculate the inverse of the matrix, return the inverse of the matrix (calculated on first
##                   call for a matrix or from a cache for susequent calls).
##    Usage:
##            m <- makeCacheMatrix()    - Setup the makeCacheMatrix object, assign to variable v.
##
##            m$set(matrix(1:4, nrow = 2, ncol = 2))
##                                      - Create the matrix (in this case, I have created a matrix of the values 1 - 4)
##
##            m$get()                   - Retrieve the matrix.
##
##            m$setInverse(i)           - Set the inverse of the matrix to i.
##
##            m$getInverse()            - Retrieve the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  # Although it is not enforced in code, x should be a matrix that can be inverted.
  # This means that the matrix must be square (have the same number of rows and columns),
  # and the matix's determinent must not be 0.
  # See https://en.wikipedia.org/wiki/Invertible_matrix for more details.
  # 
  i <- NULL # i will contain the inverse of the matrix x
  set <- function(y) {
    x <<- y         # <-- directs code to search for x in enclosing environment for assignment.
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) i <<- solve # <-- directs code to walk up the function 
                                            # definition path (enclosing environment) to find a 
                                            # definition for solve().
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve      - Use the makeCacheMatrix functions to get the matrix and check to see if the inverse of the
##                   matrix has been previously calculated and cached. If the inverse is cached, retrieve it. If
##                   not, calculate the invers of the matrix and cache it. In either case, return rhe inverse of 
##                   the matrix.
##
##    Usage:
##
##      cacheSolve(v) - v is the object created with makeCacheMatrix. The matrix should be defined with a call to
##                      v$set() before calling this function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}

