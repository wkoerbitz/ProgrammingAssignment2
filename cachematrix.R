## Creates a list of functions containing the data, the mean of the computation
## Function takes a square invertible matrix as an input and
makeCacheMatrix <- function(x = matrix()) {
  m   <- NULL                                                     # clears the object m 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get     <- function() x                                         # function to get the data
  setInverseMatrix <- function(inverseMatrix) m <<- inverseMatrix # function to asign input to object m
  getInverseMatrix <- function() m                                # function to retrive object m
  list(set = set, get = get,                                      # function are structured in a list
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}

## Returns inverse matrix if allready computed or computes it 
## Function takes a list from the makeCacheMatrix function as input
cacheSolve <- function(x, ...) {
  m     <- x$getInverseMatrix()        # retrieves from the input object if there is a value
  if(!is.null(m)) {                    # if inverse matrix is in m
    message("getting cached data")     # function gives inverse matrix from m as output
    return(m)                          # and ends function call
  }
  data <- x$get()                      # if there is nothing in variable m data is retrieved through the get function 
  m    <- solve(data, ...)             # inverse matrix is computed and stored in object m
  x$setInverseMatrix(m)                # asigns the inverse matrix to the object m through the setInverseMatrix
  m
}

# example:
a <- makeCacheMatrix(matrix(1:4, ncol=2))
cacheSolve(a)
