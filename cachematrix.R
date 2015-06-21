
##  It may make sense to cache the result of a time-consuming computation
##  so that when it's needed again, it can be looked up in the cache rather
##  than recomputed.  These functions take advantage of the scoping rules
##  of the R language to calculate and cache the inverse of a matrix, and
##  then take advantage of the cached result if the inverse is needed again.

##  makeCacheMatrix creates a list of functions that can get and set the 
##  cached inverse of matrix "x".  

makeCacheMatrix <- function(x = matrix()) {
        
                inv <- NULL
                set <- function(y) {
                        x <<- y
                        inv <<- NULL
                }
                get <- function() x
                setinverse <- function(newinv) inv <<- newinv  
                getinverse <- function() inv
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
        }
        

##  cacheSolve returns the inverse of matrix "x"  using the functions
##  created by makeCacheMatrix.  If the inverse has already been calculated
##  (and the matrix has not changed), then the inverse is retrieved from the
##  cache .  Otherwise, cacheSolve calculates the inverse and stores it in
##  the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
