#########################
## general description ##
#########################

### What it does?

# The couple of these two functions allow to create the inverse of a matrix.
# Moreover, they allow to cache the results, avoiding repeated calculation.

### How to test it?

# Do as follows :
# 1. create a matrix, e.g.: a <- matrix (runif(100), 10, 10)
# 2. apply function makeCacheMatrix: b <- makeCacheMatrix(a)
# 3. apply function cacheSolve on the result: cacheSolve(b)
# 4. apply this function a second time: cachesolve(b)
# 5. notice that the inverse is calculated in step 3
# 6. notice that in step 4, there was no actual calculation, because the result was cached

### first function

# this function just creates a list of 4 other functions :
# get : just get the matrix
# setsolve : this caches the inverted matrix (writo to m)
# getsolve : this gets m, which is NULL or inverted matrix
# set : allows to change x

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) m <<- solve
      getsolve <- function() m
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


### second function

# 1. apply getsolve function : m will be NULL the first time, the inverted matrix 2nd time
# 2. if m was not NULL, we can just return it, without needing to apply solve() function
# 3. if m was NULL we use the get function to get the matrix and subsequently calculate inverse
# 4. also if m was NULL we use setsolve function to cache the results

cacheSolve <- function(x, ...) {
      m <- x$getsolve()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
}
