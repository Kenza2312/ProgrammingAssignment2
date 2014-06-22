## makeCacheMatrix returns a list based on an square matrix that holds 4 items.
## points to the solve R function that is used to compute the invesr of a square matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## function return the inverse of a square matrix based on a list that is return by the MakeCacheMatrix function on any square matrix.
## If the function is called a second time with the same input it will return the inverse from the cache instead of computing it again.

cacheSolve <- function(x, ...) {
         inv_x <- x$getinv()
        if(!is.null(inv_x)) {
                message("getting cached data")
                return(inv_x)
        }
        data <- x$get()
        inv_x <- solve(data, ...)
        x$setinv(inv_x)
        inv_x
       ## Return a matrix that is the inverse of 'x'
}
