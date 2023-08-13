## The first function makeCacheMatrix creates a special matrix object and allows the following operations: setting and getting the value of the matrix+setting and getting the value of the inverse
## The second function cacheSolve returns the cached value of the inverse if it exists,if not the function calculates the inverse itself
makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    
    set_mat<-function(M){   x<<-M
                            inv<<-NULL
    }
    
    get_mat<-function() x
    
    set_inv<-function(inverse) inv<<-inverse
    
    get_inv<-function() inv
    
    
  list(set_mat=set_mat,get_mat=get_mat,set_inv=set_inv,get_inv=get_inv)
  
}



cacheSolve <- function(x,...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$get_inv()
  
  if(!is.null(inv)){
    
    return(inv)
  }
    mat<-x$get_mat
    inv<-solve(mat,...)
    x$set_inv(inv)
    inv
  
  
}
