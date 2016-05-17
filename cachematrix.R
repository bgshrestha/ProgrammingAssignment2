
makeCacheMatrix <- function(ab= matrix()) {
        ## create a matrix object ab and some associated sub-functions/methods
        
        ## define the cache m
        m <- NULL
        set <- function(cd) {
                ab <<- cd ## assign the input matrix cd to the variable ab in the
                ## parent environment
                m <<- NULL ## re-initialize m in the parent environment to null
        }
        get <- function() ab ## return the matrix ab
        setinverse <- function(inverse) m <<- inverse ## set the cache m equal
        ## to the inverse of the matrix ab
        getinverse <- function() m ## return the cached inverse of ab
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


cacheSolve <- function(ab, ...) {
        ## Return a matrix that is the inverse of 'ab'
        
        m <- ab$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- ab$get()
        m <- solve(data, ...)
        ab$setinverse(m)
        m
}