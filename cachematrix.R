## Matrix inversion is a costly computation if data size is high and it is good idea to cache the result if data set is same.
## Advantages: 
  ##No memory use second time and there after if data set is same.
  ## Fast result as it get from cache
## set the value of Matrix, get the value of Matrix
## set the value of inverse Matrix and get the value of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function checks if the inverse has already been computed. If not it compute the value and store in Cache.
## If it was already computed then it take the value from cached and skip the compute process.

cacheSolve <- function(x, ...) {
     inv <- x$getinverse()
    if(!is.null(inv)) {
        message("data fetched from cache")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
