## Put comments here that give an overall description of what your
## functions do

## Sets the constructor of the matrix cache object

makeCacheMatrix <- function(x = matrix()) {
    ## Default to NULL on creation
    inv <- NULL
    ## Assign new given matrix as global stored matrix and reset inverse
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    ## Return matrix value
    get <- function() x
    ## Assign given matrix as global matrix inverse
    setinverse <- function(solve) inv <<- solve
    ## Return stored inverse matrix
    getinverse <- function() inv
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}


## Gets the stored matrix inverse and if not found
## computes the inverse of matrix in makeCacheMatrix object 'x'

cacheSolve <- function(x, ...) {
    ## Get the inverse of 'x'
    inv <- x$getinverse()
    ## If the stored inverse is not null, return the stored inverse
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    ## In case the inverse is null, compute new inverse
    data <- x$get()
    inv <- solve(data,...)
    ## Store the new inverse in 'x'
    x$setinverse(inv)
    inv
}
