## This two functions work together to get a inverse matrix of the input one.
## but before it inverse it ,it will first check the inside environment if the inverse matrix has been
## calculated, if yes ,then just output the stored inverse matrix, if not,inverse the input matrix and 
## then stored it.
## Thanks for your reviewing!Please give me your advice!

## create a special  object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {     # the input here should be matrix(and must be square or will be error)

  m <- NULL                                     # m will be set NULL everytime makeCacheMatrix is called
  set <- function(y) {                          # take a input matrix
    x <<- y                                     # save the input matrix
    m <<- NULL                                  # reset m to NULL
  }                                             # the following three function will not be called when makeCacheMatrix is called
  get <- function() x                           # return the value of the input matrix
  setmatrix <- function(matrix) m <<- matrix    # store value use superassignment and called by catchMatrix
  getmatrix <- function() m                     # return the value to catchMatrix
  list(set = set, get = get,                    # list of functions,returned with the new object
       setmatrix = setmatrix,
       getmatrix = getmatrix)
  
}


## calculate the inverse matrix of the input matrix 
## but first check cache if null,then calculate and store it in cache,if not null,return the cached value 

cacheSolve <- function(x, ...) {                # input should be the object created by makecacheMatrix
  
  m <- x$getmatrix()                            # access the object and call getmatrix
  if(!is.null(m)) {                             # if m is not NULL then return the value of m(stored value)
    message("getting cached data")
    return(m)
  }
  data <- x$get()                               # if m is NULL
  m <- solve(data,...)                          # then calculate the inverse matrix
  x$setmatrix(m)                                # store it
  m
        ## Return a matrix that is the inverse of 'x'  
}
