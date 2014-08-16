
## makeCacheMatrix returns a created list of functions to
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        
        inverse <- NULL                    # inverse will store the cached inverse matrix
        
        set <- function(new_x) {           # Setter for the matrix
            x <<- new_x                    # store new matrix
            inverse <<- NULL               # set inverse to NULL to delete old one 
        }
        
        get <- function() {                # Getter for the matrix
            x
        }
        
        set_inverse <- function(inv) {     # Setter for the inverse
            inverse <<- inv 
        }
        
        get_inverse <- function() {        # Getter for the inverse
            inverse
        }
        
        list(set=set,                      # Return the matrix with new functions
             get=get, 
             set_inverse=set_inverse, 
             get_inverse=get_inverse)    
}






## cacheSolve returns the inverse of the matrix. 
## If it is already in the cache due to a recent call the result is reused.
## Otherwise calculate the inverse and cache it for future use.
## 
## To be guaranteed: a matrix that is invertible
cacheSolve <- function(x, ...) {

        inv <- x$get_inverse()          # try to get the inverse
        
        if (!is.null(inv)) {            # If the inverse is already calculated, return it
            message("Cache used")
            return(inv)
        }
        else   {                        # Else it is not yet calculated, we have to do it
          
            data <- x$get()             # call the matrix to be inverted
            
            inv <- solve(data, ...)     # calc the inverse and store it locally  
            
            x$set_inverse(inv)          # Store inverse for future calls
            
            inv                         # Return it
        }
}


## Example:
## Create a simple Matrix
## > x <- matrix(c(2,1,5,3), nrow = 2, ncol = 2)  
## Call the function to build a matrix 
## > cacheX <- makeCacheMatrix(x)
## Test if it is stored 
## > cacheX$get()
## [,1] [,2]
## [1,]    2    5
## [2,]    1    3
## First try for inverse
## > cacheSolve(cacheX) 
## [,1] [,2]
## [1,]    3   -5
## [2,]   -1    2
## Solve it again, and use the cache
## > cacheSolve(cacheX) 
## Cache used
## [,1] [,2]
## [1,]    3   -5
## [2,]   -1    2
##




