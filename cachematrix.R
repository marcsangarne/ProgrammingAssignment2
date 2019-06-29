

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse_matrix <- NULL
        set_matrix <- function(y) {
                x <<- y
                inverse_matrix <<- NULL
        }
        get_matrix <- function() x
        cache_inverse <- function(y) inverse_matrix <<- y
        get_cache <- function() inverse_matrix
        list(set_matrix = set_matrix, get_matrix = get_matrix, cache_inverse = cache_inverse, get_cache = get_cache)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_matrix <- x$get_cache()
        if(!is.null(inverse_matrix)) {
                message("getting cached data")
                return(inverse_matrix)
        }
        data <- x$get_matrix()
        inverse_matrix <- solve(data,...)
        x$cache_inverse(inverse_matrix)
        inverse_matrix
}