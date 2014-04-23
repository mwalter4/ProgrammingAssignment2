##These functions compute the inverse of a matrix.  To speed
## up computation time, the inverse of the matrix is
## cached to avoid unnecessary computations.

##This function creates a "matrix", which 
## is really just a list which contains a function
## to 1. set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <-function(solve){ inv <<- solve}
        getinv <- function(){inv}
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function calculates the inverse of the "matrix" given in
## the above function.  It checks to see if the inverse has been 
## calculated already.  If so, it takes the inverse from the cache
## If not, it calculates the inverse and sets the value of the 
##inverse with the setinv function

cacheSolve <- function(x, ...) {
                inv <- makeCacheMatrix(x)$getinv()
                if(!is.null(inv)) {
                        message("getting cached data")
                        return(inv)
                }
                data <- makeCacheMatrix(x)$get()
                inv <- solve(data, ...)
                makeCacheMatrix(x)$setinv(inv)
                inv
        
}
