## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
      inverse_mat <- NULL ## here the inverse is stored, set to NULL each time is called
      
      ##  The next three functions ("methods") are not run when makeCacheMatrix is called.
      ##  They function are used by cacheSolve() to get values for x for the inverse matrix
      ##  inverse_mat (solve) and for setting the inverse matrix. 
      
      get_original_matrix <- function() { x }               ## this function returns the value of the original matrix
      
      set_inverse <- function(solve)                        ## Called by cacheSolve() during the first cacheSolve() access
      { inverse_mat <<- solve }                             ##  Store the value using superassignment
      
      get_inverse <- function() { inverse_mat }             ## Return the cached value to cacheSolve() on next access
   
      
      list(get_original_matrix = get_original_matrix,       ##  This list is returned with the newly created object.       
           set_inverse = set_inverse,                       ##   It lists all the functions ("methods") that are part of
           get_inverse = get_inverse)                       ##   the object.  If a function is not on the list then it cannot
                                                            ##   be accessed externally.

}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

      ## Return a matrix that is the inverse of 'x'
      inverse_mat <- x$get_inverse()                        ## accesses the object 'x' and gets the inverse matrix
      if(!is.null(inverse_mat)) {                           ## if the inverse matrix was already cached (not NULL) 
            
            message("getting cached data for inv matrix")   ## print message to the console
            return(inverse_mat)                             ## Return the inverse matrix
           
      }
      data <- x$get_original_matrix()                       ## This code is reached only if x$get_inverse() returned NULL
      inverse_mat <- solve(data, ...)                       ## if inverse_mat was NULL then we have to use solve to get the inverse matrix
      x$set_inverse(inverse_mat)                            ## store inverse matrix in x 
      inverse_mat                                           ## return the inverse matrix
      
}


# test output
#>  c=rbind(c(1, -1/4), c(-1/4, 1))  
#> c
#      [,1]  [,2]
#[1,]  1.00 -0.25
#[2,] -0.25  1.00
#> test2 <- makeCacheMatrix(c)
#> cacheSolve(test2)
#          [,1]      [,2]
#[1,] 1.0666667 0.2666667
#[2,] 0.2666667 1.0666667
#> cacheSolve(test2)
#getting cached data for inv matrix
#          [,1]      [,2]
#[1,] 1.0666667 0.2666667
#[2,] 0.2666667 1.0666667
