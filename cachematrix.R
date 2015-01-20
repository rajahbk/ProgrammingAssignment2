## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly.
## So these function first compute the inverse of a matrix and cache it, if inverse of the same matrix is needed it
## doesnt compute its inverse again
## rather print the value that is already in cache.

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache 
## via the setinverse function.

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data)
      x$setinverse(i)
      i
}

##test
## >mymat <- matrix(c(2, 8, 7, 6, 12, 33, 7, 0, 35), 3, 3)       created a matrix called mymat
## > mymat                                                       print mymat
##[,1] [,2] [,3]
##[1,]    2    6    7
##[2,]    8   12    0
##[3,]    7   33   35
##> x <- makeCacheMatrix(mymat)                                  using the first function
##> cacheSolve(x)                                                using the second function
##[,1]        [,2]        [,3]
##[1,]  1.0000000  0.05000000 -0.20000000
##[2,] -0.6666667  0.05000000  0.13333333
##[3,]  0.4285714 -0.05714286 -0.05714286
##> z <-cacheSolve(x)                                            storing the result in 'z'        
##getting cached data
##> mymat%*%z                                                    checking the inverse
##[,1]         [,2]          [,3]
##[1,] 1.000000e+00 3.469447e-17 -2.081668e-17
##[2,] 4.440892e-16 1.000000e+00 -1.110223e-16
##[3,] 9.436896e-16 6.245005e-17  1.000000e+00
