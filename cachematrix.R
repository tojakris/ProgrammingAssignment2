## Put comments here that give an overall description of what your
## functions do:


## Below functions cache and compute the matrix inversion as it is usually a costly computation 
## and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly


## Below function  creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {                        ## defining the set function
                x <<- y
                m <<- NULL
        }
        get <- function() x                         ## defining the get function
        setInverse <- function(solve) m <<- solve   ## defining the setInverse function
        getInverse <- function() m                  ## defining the getInverse function
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)               ## returning the data as the list
}

}


## Below function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {

        m <- x$getInverse()
        if(!is.null(m)) {         ## checking if the matrix inverse is already in the memory
                message("getting cached matrix inverse")
                return(m)         ## Return a matrix that is cached
        }
        data <- x$get()
        m <- solve(data, ...)     ## Calculating the inverse of the matrix
        x$setInverse(m)
        m                         ## Returning a matrix that is the inverse of 'x'
}
        
}
