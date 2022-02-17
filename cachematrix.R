## From a given an invertible matrix calculate its inverse and
## cache the inverse of that matrix


## Creates a special "matrix" object that can cache it inverse
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      # retrieve x from parent environment
      get <- function() x
      # calculate the inverse
      setinverse <- function() m <<- solve(x)
      # get the inverse (m) from parent environment
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}



## Computes the inverse of the special matrix returned by makeCacheMatrix
## function above. If inverse has already been calculates (and the matrix
## has not changed), then this function bellow should retrive the inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if (!is.null(m)) {
            message("getting cached data..")
      }
      data <- x$get()
      m <- solve(data)
      x$setinverse()
      m
      
}


## Testing the functions
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
m1
I2 <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
I2
n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
# checks:
m1 %*% n1
n1 %*% m1
solve(m1)
solve(n1)

my_matrix_obj <- makeCacheMatrix(m1)
cacheSolve(my_matrix_obj)
cacheSolve(my_matrix_obj)
n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
my_matrix_obj$set(n2)
my_matrix_obj$get()
cacheSolve(my_matrix_obj)
cacheSolve(my_matrix_obj)




