# This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        #initializes inverse as NULL
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv<<-NULL
        }
        # gets matrix x
        get <- function()x
        setinv <- function(inverse) inv<<-inverse
        getinv<-function() {
                inv
        }
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated(and the matrix 
# has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        # checks if inverse is null or not
        if(!is.null(inv)) {
                message("retrieving cached data!")
                return(inv)
        }
        data <- x$get()
        # gets inverse value
        inv <- solve(data,...)
        x$setinv(inv)
        ## Return a matrix that is the inverse of 'x'
        inv
}