## Pair of functions assignment that cache the inverse of a matrix. 
## Jason McKenna
## 30 August 2017

## Function to create a matrix object that can cache the inverse of a 
## specified (invertible matrix) using solve()
##
## workflow is: output<-makeCacheMatrix(matrix(c(1,2,3,4), 2,2)) #this matrix is invertible
## then: cacheSolve(output)
## exeucting cacheSolve(output) twice will show that the matrix inverse is read from its cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #
  get <- function() x
  setmean <- function(solve) m <<- solve
  getmean <- function() m
  #  
  list(set = set,          # gives the name 'set' to the set() function defined above
       get = get,          # gives the name 'get' to the get() function defined above
       setmean = setmean,  # gives the name 'setmean' to the setmean() function defined above
       getmean = getmean)  # gives the name 'getmean' to the getmean() function defined above
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache as seen below
##
# > source('C:/Users/JRM/ProgrammingAssignment2/cachematrix.R')
# > output<-makeCacheMatrix(matrix(c(1,2,3,4), 2,2))
# > cacheSolve(output)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(output)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getmean()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setmean(m)
    m
  }
