## There are two functions in this set.  The first (makeCacheMatrix) creates a matrix object 
## that can cache its inverse.  The second (cacheSolve) computes the inverse of makeCacheMatrix,
## returning an already computed value if one exists.

## makeCacheMatrix creates a "matrix" object that converts an existing matrix into a form 
## that allows for the inverse to be determined (using "solve") and then cached. 

makeCacheMatrix <- function(x = matrix()) {
            s <- NULL
            set <- function(y) {
              x <<- y
              s <<- NULL
            }     
            get <- function()x
            setsolve <- function(solve) s <<- solve
            getsolve <- function() s
            list(set = set, get = get,
                 setsolve = setsolve,
                 getsolve = getsolve)
          
}


## This function elicits the inverse of the matrix 'x' by using the "solve" function, 
## checking to see if the result is stored in cache. If so, the function returns the cached value.  
## Otherwise it calculates the inverse and sets that in cache.

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)){
            message("Time to get the cached data!")
            return(s)
        }
        else data <-x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
