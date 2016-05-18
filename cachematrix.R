## makeCacheMatrix() and cacheSolve() together executes matrix inversion and cashes the resulting
## inverse and if an attempt is made to solve for the inversion that has already been solved for the cached
## inverse is returned to avoid repeating the same calculation


## makeCacheMatrix() 
## creates a matrix object that can cache the its inverse
makeCacheMatrix <- function(x = matrix()) {
        
        # initializes inv as NULL
        inv <- NULL
        
        # sets the value of the matrix and sets inv as NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # gets the matrix
        get <- function() x
        
        # caches the inverse
        setinv <- function(inverse) inv <<- inverse
        
        # gets the inverse from the cache
        getinv <- function() inv
        
        # returns a list of above defined functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
        
}


## cacheSolve() 
## uses the functions defined in makeCacheMatrix() (except set()) to either
## get cached inverse of matrix (if cached) or solve for inverse and then cache it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()       # gets inverse
        if(!is.null(inv)) {     # checks if inverse is cached, if yes: message and inverse is returned
                message("getting cached data")
                return(inv)
        }
        # if inverse is not cached: 
        data <- x$get()     # gets matrix
        inv <- solve(data, ...) # solves for the inverse of the matrix
        x$setinv(inv)           # inverse is cached
        inv                     # inverse is returned
}
