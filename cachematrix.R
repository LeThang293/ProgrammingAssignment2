# The first function,  makeMatrix  creates a special "matrix", which is really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse using solve function 
# 4. get the value of the inverse


makeCacheMatrix  <- function(x = matrix()) { 
  m <- NULL 
  set <- function(y) { 
    x <<- y 
    m <<- NULL 
  } 
  get <- function() x 
  setinverse <- function(solve) m <<- solve 
  getinverse <- function() m 
  list(set=set, get=get, 
       setinverse=setinverse,
       getinverse=getinverse) 
}

# The following function returns the inverse of the matrix. It first checks if 
# the inverse has already been computed. If so, it gets the result and skips the 
# computation. If not, it computes the inverse, sets the value in the cache via 
# setinverse function. 
# This function assumes that the matrix is always invertible. 

cacheSolve <- function(x, ...) { 
  m <- x$getinverse() 
  if(!is.null(m)) { 
    message("getting cached data.") 
    return(m) 
  } 
  data <- x$get() 
  m <- solve(data) 
  x$setinverse(m) 
  m 
} 

# Test 
#> x = rbind(c(1, 2), c(3, 5))
#> m = makeCacheMatrix(x)
#> m$get()
#     [,1] [,2]
#[1,]    1    2
#[2,]    3    5
#> cacheSolve(m)
#     [,1] [,2]
#[1,]   -5    2
#[2,]    3   -1
