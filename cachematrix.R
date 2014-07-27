## A pair of functions that cache the inverse of a matrix


makeCacheMatrix <- function(x=matrix()){
        ## This function creates a special "matrix" object that can cache its inverse
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve) 
        
}

## This function inverses the cached data
cacheSolve <- function(x, ...){
       
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- (solve(data, ...))
        x$setsolve(m)
        m
}