##Assignment 2 - R Programming 



##Our goal is to get the inverse matrix of a given one

## In order to do this we will evaluate whether this inverse
## matrix has already been calculated and stored or not, so we
## avoid wasting extra time for these calculations.



## We will need two different functions. The first one will define
## four smaller functions that will:

##    1. Set the target matrix and store it
##    2. Get the target matrix from the memory position where it has been stored.
##    3. Set the inverse of the target matrix and store it
##    4. Get this inverse from the memory position where it has been stored.

## Then the output is a list containing these four functions.

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialization of the variable where we will store the inverse matrix
  i <- NULL  
  
  ## Function 1: Set the target matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }

  ## Function 2: Get the target matrix
  get <- function() {
    x
  }
  
  ## Function 3: Set the inverse matrix
  setinverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Function 4: Set the inverse matrix
  getinverse <- function(){
    i
  }
  
  ## Return a list containing all the four functions
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}


## The second function will use the four functions defined above to:
##    1. Check if the inverse of the current target has already been calculated and stored
##    2. If it has been previously calculated, then get it from the memory and print it.
##    3. If it hasn't been calculated yet, then calculate it and print it.

## The final output is the inverse of our target matrix.

cacheSolve <- function(x, ...) {
       
  ## Try to retrieve the inverse matrix from the cache
  i <- x$getinverse() 
  
  ## If it can retrieve the inverse matrix from the cache, return it and the function ends
  if(!is.null(i)) {
    message("Getting cached data.")
    return(i)
  }
  
  ## If it hasn't been able to get the inverse from the cache, get the target matrix and
  ## calculate the inverse. Afterwards, store it in the cache and print it.
  
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
  
}


## Test of the results

## source("cachematrix.R")
## x = rbind(c(1, -1/2), c(-1/2, 1))
## m <- makeCacheMatrix(x)
## m$get()

##        [,1] [,2]
##   [1,]  1.0 -0.5
##   [2,] -0.5  1.0

## cacheSolve(m)

##             [,1]      [,2]
##   [1,] 1.3333333 0.6666667
##   [2,] 0.6666667 1.3333333

