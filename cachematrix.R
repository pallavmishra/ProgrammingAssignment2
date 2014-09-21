## This function caches the inverse of a matrix
## functions do

## Create a special object that can cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  ## initialize the inverse variable
  i <- NULL
  set <- function(matrix){
    m <<- matrix
    i <<- NULL
  }
  get <- function(){
    m
  }
  setInv <-function(inverse){
    i <<- inverse
  }
  getInv <-function(inverse){
    i
  }
  list(set = set, get = get, 
       setInv = setInv, 
       getInv = getInv)

}


## The below function computes the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <-x$get()
  ## Calculate the inverse using matrix multiplication
  m<- solve(data) %*% data
  x$setInv(m)
  ## Return the value of the matrix m
  m
}
