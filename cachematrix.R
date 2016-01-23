## The two function works together to create a matrix, 
## solve the inverse of a matrix. and store the matrix and its 
## inverse in cache
## 



makeCacheMatrix <- function(x = matrix()) {

  inv<-NULL
  
  ## create a inverse variable and set the default value to be 'NULL'
  
  
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  
  ## method to set the matrix object
  
  get<-function()x
  
  ## method to read the matrix object
  
  setinv<-function(invert) inv<<-invert
  
  ## method to set inverse variable "inv" with a value from the calling function.
  
  getinv<-function()inv
  
  ## method read the inverse varibale from the function
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
  ## return of the function is a list
  
  
}


##  this function reads object as cached from the makeCacheMatrix function 
##  and calculate the inverse if the inverse does not exit already in the 
##  cached object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv<-x$getinv()
        ## read the inverse of matrix from the cache
  
  if(!is.null(inv)){
    
      message("getting cached inverse data")
     
      return(inv)
    
      ## return the inv matrix if the inverse already exists
  }
      
  data<-x$get()
      ##store the matrix into a temporary variable
  
  inv<-solve(data,...)
  
      ## solve the matrx
  
  x$setinv(inv)
      ## set the inverse into cache
  
  inv
      ## return inverse
  
    
}
