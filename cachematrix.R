## The following functions calculates the inverse of the  "matrix" 
#It first checks to see if the inverse has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the data and sets the value of the 
#inverse in the cache via the setinverse function.



makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}



cacheSolve <- function(x, ...) {
        
   ## Return a matrix that is the inverse of 'x'

                inverse <- x$getinverse()
                if(!is.null(inverse)) {
                        message("getting cached data")
                        return(inverse)
                }
                data <- x$get()
                inverse <- solve(data, ...)
                x$setinverse(inverse)
                inverse
}

