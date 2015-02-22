## The two functions below help speed up the process of calculating the inverse of a matrix by first checking
## to see if the inverse of the matrix in question has already been calculated and stored. If the inverse has 
## already been calculated then the two functions return the inverse without having to take the time to calculate it.
## If the inverse has not already been calculated it calculates the inverse, stores the result, and returns the inverse.

## The makeCacheMatrix function creates 4 functions that are used to set the value of the matrix, pull the value of the matrix,
## set the inverse of that matrix, and then to pull the inverse of that matrix. All of these functions are used 
##in the "cacheSolve" function

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get <-function(){
    x
  }
  set_inv<-function(inv){
    m<<-inv
  }
  get_inv<-function(){
    m
  }
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}


## The cacheSolve function uses the 4 functions created in "makeCacheMatrix" to first check to see if the inverse for the
## matrix given to the "makeCacheMatrix" function has already been caluclated. If it has, it returns the inverse wihtout having
## to calculate it. If the inverse for that function has not been calculated previously it then calculates the inverse
## and returns the inverse.

cacheSolve <- function(x, ...) {
  
  m<-x$get_inv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$set_inv(m)
  m
}
