# function to retrieve list of functions to apply to input matrix
# we assume input matrix is always invertible
makeCacheMatrix <- function(input_matrix) {
  inverse_matrix <- NULL
  
  # set
  set <- function(y) {
    input_matrix <<- y
    inverse_matrix <<- NULL
  }
  
  # get 
  get <- function() input_matrix
  
  #set inverse matrix
  set_inverse <- function(m_inverse) inverse_matrix <<- m_inverse
  
  # get inverse matrix
  get_inverse <- function() inverse_matrix
  
  # output list
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

# function to retrieve inverse of matrix from cache in case it exists
# otherwise inverse is calculated

cacheSolve <- function(x, ...) {
  m <- x$get_inverse()
  if(!is.null(m)) {
    # returning cached data
    print("returning cached data")
    return(m)
  }
  else {
    # if there is no cached data, let's calculate the inverse matrix
    data <- x$get()
    m <- solve(data, ...)
    # let's keep the matrix in the cache for future calls
    x$set_inverse(m)
    # returning recently calculated inverse matrix
    return(m)
  }
}