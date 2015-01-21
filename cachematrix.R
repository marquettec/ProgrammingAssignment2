## Matrix inverse computation function
## Those function also caches latest processed inverse matrix to increase performance


## Function stores matrix and its inverse
## Returned list provides function allowing to set/get both matrices

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverseMatrix) i <<- inverseMatrix
    getinverse <- function() i
    list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


## Function returns x inverse matrix if in cache, else computes it and returns it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
