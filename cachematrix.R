## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
            x <<- y
            m <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) m <<- inverse
    getInv <- function() m
    list(set = set, 
         get = get,
         setInv = setInv,
         getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInv()
    if(!is.null(m)) {
            message("getting cached matrix")
            return(m)
    }
    
    data <- x$get()
    
    m <- solve(data, ...)
    
    x$setInv(m)
    m        
}


