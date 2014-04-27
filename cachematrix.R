
makeCacheMatrix <- function(x = matrix()) {
    x.inv <- NULL # Inverse of x
    set <- function(y) {
        x <<- y
        x.inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inv.mat) x.inv <<- inv.mat
    getInverse <- function() x.inv
    return(list(set = set, get = get, setInverse = setInverse,
                getInverse = getInverse))
}


cacheSolve <- function(x, ...) {
    x.inv <- x$getInverse()
    if(!is.null(x.inv)) {
        message("getting cached data")
        return(x.inv)
    }
    data <- x$get()
    x.inv <- solve(data, ...)
    x$setInverse(x.inv)
    return(x.inv)
}
