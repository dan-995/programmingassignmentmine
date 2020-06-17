
## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to

###set the value of the matrix(set)
###get the value of the matrix(get)
###set the value of the inverse of the matrix(setinv)
###get the value of the inverese of the matrix(getinv)

makeCacheMatrix <- function(x = matrix()) {
c <- NULL
set <- function(y){
  x <<- y
  c <<- NULL
}
get <-function()x
setinv <- function(inv){c <<- inv}
getinv <- function()c
list(get = get,set = set,setinv = setinv,getinv = getinv)
}


## The function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        c <- x$getinv()
        if (!is.null(c)){
          message("loading cached inverse matrix")
          return(c)
        }
        req_mat <- x$get()
        c <- solve(req_mat)
        x$setinv(c)
        c
}
###for the smooth operation of the function assign the value of makeCacheMatrix function to any variable.
###Then undergo the cacheSolve function of that variable.