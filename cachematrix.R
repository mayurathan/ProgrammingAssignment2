## R-programming assignment 2; two functions which compute the inverse of a matrix and caches the inverse
## 
## Special thanks to Bill Hilton for the elaborate explanation

## makeCacheMatrix creates an object(matrix) which caches the inverse

makeCacheMatrix <- function(m = matrix()) {
  
  inv<-NULL ##Initialise inverse(inv) variable
    
  ##Set function called by the first call of cachemean
  ##Values are stored using the superassignment "<<"
  set<-function(matrix){
    m<<-matrix
    inv<<-NULL
  }
  
  ##Return the cached value to cachmean
  get<-function(){
    m
  }
  
  setInverse<-function(inverse){
    inv<-inverse
  }
  
  getInverse<-function(){
    inv
  }
  
  ##List of internal functions
  
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse) 
}


## Compute the inverse of the matrix object made by makeCacheMatrix
## If matix has not been changed, and the inverse has already been
## calculated cacheSolve will return the cached value

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getInverse()
  
  ##if M is already cached return M, also send a little note
  
  if(!is.null(m)){
    message("retrieving cached matrix...")
    return(m)
    
  }
  
  matrix<-x$get()##If x$getInverse is null
  m<-solve(matrix) %*% matrix ##Caluclate the inverse if M was null
  x$setInverse(m) ##Store calculated value in x
  
  m ##returm matrix to what called the function
}
