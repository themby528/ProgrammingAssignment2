## The functions below will take an input such as matrix(c(1,2,3,4),2,2) and return the inverse of this matrix.
## They will first check if the inverse is already in the cache and if so return that cache value, if not the matric 
## inverse will be solve for and returned


## This function will take the user input and create matrix of the values that is store in the cache

makeCacheMatrix <- function(x = matrix()) {
    ## create m vector
    m <- NULL
    ## create the values provided as a matrix
    set <- function(y) {
            x <<- y
            m <<- NULL
    }
    ## return the matrix of the inputs supplied
    get <- function() x
    ## stores the function solve for use when pulling data from cache
    setinverse <- function(solve) m <<- solve
    ## this will print the inverse when called
    getinverse <- function() m
    ## create a list that holds the names of the items you just stored in the cache
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}


## This function will check the cache for the inverse of the defined matrix
## If found it will return the inverse from cache and if not it will create the inverse and store it for later

cacheSolve <- function(x, ...) {
    ## check if the inverse of the amtrix already exists in the cache
    m <- x$getinverse()
    ## if m is not empty (meaning there is something in the cache for inverse of X), pull the value from cache
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## get the x matrix from the cache and store it in the data variable
    data <- x$get()
    ## run the solve function from the cache with the solve function
    m <- solve(data, ...)
    ## store the inverse
    x$setinverse(m)
    m
}
