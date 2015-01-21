## Matrix inverse computation function
## Those function also caches latest processed inverse matrix to increase performance


## Function stores matrix and its inverse
## Returned list provides function allowing to set/get both matrices

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    # Functions
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverseMatrix) i <<- inverseMatrix
    getinverse <- function() i
    # Returning cache matrix functions
    list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


## Function returns x inverse matrix if in cache, else computes it and returns it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    i <- x$getinverse()
    # Is inverse already cached ?
    if(!is.null(i)) {
      #  If so, returning result from cache
      message("getting cached data")
      return(i)
    }
    # If not, computing inverse
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    # Returning inverse
    i
}

# In order to test

# # Please only run the following if needed (testing from outside this file)
# # source("cachematrix.R")
# 
# # Calling cacheSolve twice to check cache is used
# c=rbind(c(1, -1/4), c(-1/4, 1)) 
# cm <- makeCacheMatrix(c)
# ci <- cacheSolve(cm)
# str(ci)
# res = c %*% ci
# str(res)
# # Should output 'getting cached data' (as using cache)
# ci <- cacheSolve(cm)
# str(ci)
# res <- c %*% ci
# str(res)
# 
# # Calling cacheSolve with new matrix, to check cache was emptied
# c=rbind(c(1, -1/2), c(-1/2, 1)) 
# cm <- makeCacheMatrix(c)
# # Should NOT output 'getting cached data' (new matrix without inverse just created)
# ci <- cacheSolve(cm)
# str(ci)
# res <- c %*% ci
# str(res)
