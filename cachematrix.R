#######################
#PrommingAssignment 2 #
#######################

#author: noeeee

##makeCacheMatrix function
makeCacheMatrix <- function(x = matrix()) {
    x.inv <- NULL # Inverse of x
    set <- function(y) { # Set function
        x <<- y # Set x = y
        x.inv <<- NULL
    }
    get <- function() x # get function
    setInverse <- function(inv.mat) x.inv <<- inv.mat #Set inverse function
    getInverse <- function() x.inv #Get inverse function
    return(list(set = set, get = get, setInverse = setInverse,
                getInverse = getInverse)) #returns variables
}

## CacheSolve function
cacheSolve <- function(x, ...) {
    x.inv <- x$getInverse() # get x.inv from makeCacheMatrix
    if(!is.null(x.inv)) { # if inverse of x is not null
        message("getting cached data") #Print out message
        return(x.inv) # returns inverse matrix x
    }
    data <- x$get() # get x from makeCacheMatrix and save it into data
    x.inv <- solve(data, ...) #calculate inverse
    x$setInverse(x.inv) # Set inverse
    return(x.inv) #returns matrix x
}
