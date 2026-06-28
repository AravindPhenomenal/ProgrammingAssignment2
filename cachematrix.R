#function to create matrix , other function like get matrix , set matrix, get and set inverse
makeCachedMatrix <- function(matrix=matrix()){
  #lexical scoping 
  inverse <- NULL
  set <- function(y){
    matrix <<- y
    inverse <<- NULL
  }
  
  get <- function(){
    return(matrix)
  }
  
  set_inverse <- function(inv){
    inverse <<- inv
  }
  
  get_inverse <- function(){
    return(inverse)
  }
  
  list(set=set,get=get,set_inverse=set_inverse,get_inverse=get_inverse)
}

#function to calculate the inverse and store in cache
cachesolve <- function(spmatrix,...){
  inverse <- spmatrix$get_inverse()
  if(!is.null(inverse)){
    print("getting inverse data fomr cache!")
    return(inverse)
  }
  data <- spmatrix$get()
  inverse <- solve(data, ...)
  spmatrix$set_inverse(inverse)
  inverse
}


cacheSolve <- function(special_matrix, ...) {
  inverse <- special_matrix$get_inverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- special_matrix$get()
  inverse <- solve(data, ...)
  special_matrix$set_inverse(inverse)
  print(inverse)
}

A <- makeCachedMatrix(matrix(c(1,2,3,4),nrow=2,ncol=2))

A$get()

A$get_inverse()

cacheSolve(A)
