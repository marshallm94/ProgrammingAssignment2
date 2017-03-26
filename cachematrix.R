## These two functions, broadly speaking, work together to calculate the
## inverse of a matrix and store that inverse for later use so as to save
## memory, computing power and time if that inverse is needed in the future.

## makeCacheMatrix takes the argument x and does 4 main things:
##    1. Clears any previously cached data called by cacheSolve by setting M 
##       equal to NULL and y equal to x, the argument given in the parent
##       environment.
##    2. After clearing any cached data, makeCacheMatrix calculates the inverse
##       of the matrix x using solve(x) and assigns it to m in the parent
##       environment. (this step is defined as setinverse)
##    3. makeCacheMatrix then creates the function getinverse to return m 
##       whenever getinverse is called.
##    4. Importantly, a list is created that names each of the above operations
##       of makeCacheMatrix so that they can be called directly by their name.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function() m <<- solve(x)
      getinverse <- function() m
      list(set = set, get = get, setinverse = setinverse, 
           getinverse = getinverse)
}


## cacheSolve does a couple important operations as well:
##    1. The first thing this function does is references the getinverse() fx
##       defined in the makeCacheMatrix function and defines m as the inverse of
##       x (the argument passed to both makeCacheMatrix and cacheSolve).
##    2. If m is not NULL, then it returns a message reading, "retrieving cached
##       inverse..." and then prints m, the inverse of x.
##    3. If m is NULL, then cacheSolve defines data as x$get(), which references
##       the get function in makeCacheMatrix
##    4. Then, it defines m by calling solve() on data, giving you the inverse,
##       and caches that inverse by using x$setinverse(m). At the very end, it
##       return m 

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.na(m)) {
            message("retrieving cached inverse...")
            return(m)
      }
      data <- x$get()
      m <-solve(data,...)
      x$setinverse(m)
      m
}
