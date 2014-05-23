
### Introduction

# This second programming assignment requires an R
# function that is able to cache potentially time-consuming computations.
# For example, taking the mean of a numeric vector is typically a fast
# operation. However, for a very long vector, it may take too long to
# compute the mean, especially if it has to be computed repeatedly (e.g.
# in a loop). If the contents of a vector are not changing, it may make
# sense to cache the value of the mean so that when we need it again, it
# can be looked up in the cache rather than recomputed. 

# The below functions demonstrate scoping rules of the R language to preserve 
# state inside an R object.

### Assignment: Caching the Inverse of a Matrix

# In this assignement the `<<-` operator is used to
# assign a value to an object in an environment that is different from the
# current environment. Below are two functions that are used to create a
# special object that stores a matrix and caches its inverse.

# The first function, `makeCacheMatrix` creates a special "vector", which is
# really a list containing functions to

# 1.  set the matrix
# 2.  get the matrix
# 3.  set the inverse matrix
# 4.  get the inverse matrix


rm(list=ls())
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# The following function creates the inverse of the matrix
# created with the above function. However, it first checks to see if the
# inverse has already been calculated. If so, it `get`s the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the data and sets inverse in the cache via the `setinverse`
# function.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

