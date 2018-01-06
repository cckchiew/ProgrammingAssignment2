## 
## 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        ## set value of vector
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## get value of vector
        get <- function() x
        
        ## set value of inverse vector
        set_inv <- function(inverse) m <<- inverse
        
        ## get value of inverse vector
        get_inv <- function() m
        
        list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv)
}
        



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
        ## get inverse matrix
        m <- x$get_inv()
        
        ## if m is not NULL, retrieve data
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## get data from x$get()
        data <- x$get()
        
        ## supposely inverse data
        m <- solve(data, ...)
        
        ## Return a matrix that is the inverse of 'x'
        x$set_inv(m)
        m
}
